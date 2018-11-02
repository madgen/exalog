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

module Language.Exalog.SemiNaive where

import Protolude hiding (head, pred)

import Control.Monad.Trans.State (StateT)

import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Delta
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Unification as U

type SemiNaiveM ann = StateT (R.Solution ann) IO

-- |Temporarily execute in a different state
withDifferentState :: Monad m
                   => (r -> s) -> (s -> r) -> StateT s m a -> StateT r m a
withDifferentState inputMap outputMap (StateT f) =
  StateT $ fmap (second outputMap) . f . inputMap

semiNaive :: forall a. Eq (PredicateAnn a) => Program a -> SemiNaiveM a ()
semiNaive pr = do
  initEDBM
  withDifferentState (mkDeltaSolution revPr) cleanDeltaSolution $ do
    initEDB <- get
    (`fix` initEDB) $ \f sol -> do
      put sol
      step
      betterSol <- get
      if areAllDeltaEmpty betterSol
        then put betterSol
        else f betterSol
  where
  revPr = reverseClauses pr

  -- A simple clause is one without references to IDB predicates in its body.
  simpleClss = flip filter (clauses revPr) $ \Clause{body = body} ->
    not . any ((`elem` intentionals) . predicateBox) $ body

  -- Simple clauses can be executed without the need for fixpoint
  -- computation.
  initEDBM :: SemiNaiveM a ()
  initEDBM = execClauses simpleClss

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
    let ts = R.findTuples edb (mkDeltaPredicate Delta p)
        tsPrev = R.findTuples edb (mkDeltaPredicate Prev p)
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
      Delta -> ts `T.difference` R.findTuples edb (updateDecor Normal p)
      _ -> ts

  step :: SemiNaiveM ('ADelta a) ()
  step = do
    modify $ updateFromDelta . shiftPrevs . elimDecor PrevX2
    execClauses (clauses deltaPr)
    modify $ axeDeltaRedundancies

execClauses :: Eq (PredicateAnn a) => [ Clause a ] -> SemiNaiveM a ()
execClauses clss = do
  edb <- get
  rels <- mapM execClause clss
  put $ foldr R.add edb rels

execClause :: forall a. Eq (PredicateAnn a)
           => Clause a -> SemiNaiveM a (R.Relation a)
execClause Clause{..} = deriveHead <$> foldrM walkBody [ U.empty ] body
  where
  deriveHead :: [ U.Unifier ] -> R.Relation a
  deriveHead unifiers
    | Literal{predicate = pred, terms = terms} <- head =
      let preTuples = map (`U.substitute` terms) unifiers
      in R.Relation pred . T.fromList $ flip map preTuples $ \preTuple ->
        fromMaybe (panic "Range-restriction is violated.")
                  (extractTuples preTuple)

  walkBody :: Literal a -> [ U.Unifier ] -> SemiNaiveM a [ U.Unifier ]
  walkBody lit unifiers = fmap (catMaybes . concat) $ sequence $ do
    unifier <- unifiers
    return $ fmap (`U.extend` unifier)
         <$> execLiteral (unifier `U.substitute` lit)

execLiteral :: Eq (PredicateAnn a) => Literal a -> SemiNaiveM a [ U.Unifier ]
execLiteral Literal{predicate = p@Predicate{nature = nature}, ..}
  | Extralogical foreignAction <- nature = do
    eTuples <- lift $ foreignAction terms
    case eTuples of
      Right tuples -> return $ handleTuples terms tuples
      Left msg -> panic $ "Fatal foreign function error: " <> msg
  | otherwise = do
    edb <- get
    return . handleTuples terms . T.toList $ R.findTuples edb p
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
