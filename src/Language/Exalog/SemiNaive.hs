{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Exalog.SemiNaive
  ( semiNaive
  , SemiNaive
  , evalSemiNaiveT
  ) where

import Protolude hiding (head, pred)

import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Delta
import           Language.Exalog.Logger
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Unification as U

type SemiNaiveT ann = ReaderT (R.Solution ann)
type SemiNaive  ann = SemiNaiveT ann (LoggerT IO)

evalSemiNaiveT :: SemiNaiveT ann m a -> R.Solution ann -> m a
evalSemiNaiveT = runReaderT

withDifferentEnvironment :: Monad m
                         => (r -> s) -> ReaderT s m a -> ReaderT r m a
withDifferentEnvironment envMap (ReaderT f) =
  ReaderT $ f . envMap

semiNaive :: forall a. Eq (PredicateAnn a)
          => Program a -> SemiNaive a (R.Solution a)
semiNaive pr = do
  initEDB <- initEDBM
  local (const initEDB) $
    withDifferentEnvironment (mkDeltaSolution revPr) $ do
      initEDB <- ask
      (`fix` initEDB) $ \f sol -> do
        betterSol <- local (const sol) step
        if areAllDeltaEmpty betterSol
          then return . cleanDeltaSolution $ betterSol
          else f betterSol
  where
  revPr = reverseClauses pr

  -- A simple clause is one without references to IDB predicates in its body.
  simpleClss = flip filter (clauses revPr) $ \Clause{body = body} ->
    not . any ((`elem` intentionals) . predicateBox) $ body

  -- Simple clauses can be executed without the need for fixpoint
  -- computation.
  initEDBM :: SemiNaive a (R.Solution a)
  initEDBM = evalClauses simpleClss

  intentionals :: [ PredicateBox a ]
  intentionals = findIntentionals revPr

  deltaPr :: Program ('ADelta a)
  deltaPr = mkDeltaProgram revPr

  areAllDeltaEmpty :: R.Solution ('ADelta a) -> Bool
  areAllDeltaEmpty =
      R.isEmpty
    . R.filter (\(R.Relation p ts) -> decor p == Delta && (not . T.isEmpty) ts)

  -- Adds the current deltas to the normal version of the relation.
  -- Basicall S_{i+1} = S_i \cup delta S_i
  updateFromDelta :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  updateFromDelta edb = foldr updateFromDelta' edb intentionals

  updateFromDelta' :: PredicateBox a
                   -> R.Solution ('ADelta a)
                   -> R.Solution ('ADelta a)
  updateFromDelta' (PredicateBox p) edb =
    let ts = R.findTuples (mkDeltaPredicate Delta p) edb
        tsPrev = R.findTuples (mkDeltaPredicate Prev p) edb
    in R.add (R.Relation (mkDeltaPredicate Normal p) (ts <> tsPrev)) edb

  -- Sets Prev to PrevX2, Normal to Prev
  shiftPrevs :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  shiftPrevs edb = (`R.rename` edb) $ \p ->
    -- This is stupidly inefficient
    if PredicateBox (peel p) `elem` intentionals
      then
        case decor p of
          Normal -> updateDecor Prev   p
          Prev   -> updateDecor PrevX2 p
          _      -> p
      else
        p

  axeDeltaRedundancies :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  axeDeltaRedundancies edb = (`R.atEach` edb) $ \(p, ts) ->
    case decor p of
      Delta -> ts `T.difference` R.findTuples (updateDecor Normal p) edb
      _ -> ts

  step :: SemiNaive ('ADelta a) (R.Solution ('ADelta a))
  step = do
    let evalClauses' = evalClauses (clauses deltaPr)
    let maintenance = updateFromDelta . shiftPrevs . elimDecor PrevX2
    axeDeltaRedundancies <$> local maintenance evalClauses'

evalClauses :: Eq (PredicateAnn a)
            => [ Clause a ] -> SemiNaive a (R.Solution a)
evalClauses clss = do
  rels <- mapM evalClause clss
  edb <- ask
  return $ foldr R.add edb rels

evalClause :: forall a. Eq (PredicateAnn a)
           => Clause a -> SemiNaive a (R.Relation a)
evalClause Clause{..} = deriveHead =<< foldrM walkBody [ U.empty ] body
  where
  deriveHead :: [ U.Unifier ] -> SemiNaive a (R.Relation a)
  deriveHead unifiers
    | Literal{predicate = pred, terms = terms} <- head = do
      let preTuples = map (`U.substitute` terms) unifiers
      tuples <- (`traverse` preTuples) $ \preTuple -> do
        maybe (lift $ scold Nothing "Range-restriction is violated.")
              pure
              (extractTuples preTuple)
      pure $ R.Relation pred . T.fromList $ tuples

  walkBody :: Literal a -> [ U.Unifier ] -> SemiNaive a [ U.Unifier ]
  walkBody lit unifiers = fmap (catMaybes . concat) $ sequence $ do
    unifier <- unifiers
    return $ fmap (`U.extend` unifier)
         <$> execLiteral (unifier `U.substitute` lit)

execLiteral :: Eq (PredicateAnn a) => Literal a -> SemiNaive a [ U.Unifier ]
execLiteral Literal{predicate = p@Predicate{nature = nature}, ..}
  | Extralogical foreignAction <- nature = do
    eTuples <- liftIO $ foreignAction terms
    case eTuples of
      Right tuples -> return $ handleTuples terms tuples
      Left msg -> lift $ scold Nothing $ "Fatal foreign function error: " <> msg
  | otherwise =
    handleTuples terms . T.toList . R.findTuples p <$> ask
  where
  handleTuples :: V.Vector n Term -> [ V.Vector n Sym ] -> [ U.Unifier ]
  handleTuples terms tuples =
    case polarity of
      Positive -> tuplesToUnifiers terms tuples
      Negative -> [ U.empty | null (tuplesToUnifiers terms tuples) ]

  tuplesToUnifiers :: V.Vector n Term -> [ V.Vector n Sym ] -> [ U.Unifier ]
  tuplesToUnifiers terms = mapMaybe (U.unify terms)

extractTuples :: V.Vector n Term -> Maybe (V.Vector n Sym)
extractTuples = traverse (\case
  TSym sym -> Just sym
  TVar{}   -> Nothing)

reverseClauses :: Program a -> Program a
reverseClauses pr = pr { clauses = map reverseClause . clauses $ pr}
  where
  reverseClause :: Clause a -> Clause a
  reverseClause cl = cl { body = NE.reverse . body $ cl }
