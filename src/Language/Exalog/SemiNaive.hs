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

import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Delta
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Unification as U

semiNaive :: forall a. Eq (PredicateAnn a)
          => Program a -> R.Solution a -> IO (R.Solution a)
semiNaive pr edb = do
  initEDB <- initEDBM
  sol <- (`fix` initEDB) $ \f sol -> do
    betterSol <- step sol
    if areAllDeltaEmpty betterSol
      then return betterSol
      else f betterSol
  return $ cleanDeltaSolution sol
  where
  revPr = reverseClauses pr

  -- A simple clause is one without references to IDB predicates in its body.
  simpleClss = flip filter (clauses revPr) $ \Clause{body = body} ->
    not . any ((`elem` intentionals) . predicateBox) $ body

  initEDBM :: IO (R.Solution ('ADelta a))
  initEDBM = do
    edb <- execClauses simpleClss edb
    let deltaSol = mkDeltaSolution revPr $ edb
    return $ (`R.atEach` deltaSol) $ \case
      (p, ts)
        | Prev   <- decor p -> R.findTuples deltaSol (updateDecor Delta p)
        | PrevX2 <- decor p -> R.findTuples deltaSol (updateDecor Delta p)
        | otherwise -> ts

  intentionals :: [ PredicateBox a ]
  intentionals = findIntentionals revPr

  deltaPr :: Program ('ADelta a)
  deltaPr = mkDeltaProgram revPr

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

  -- Sets PrevX2 to Prev, Prev to Normal
  shiftPrevs :: R.Solution ('ADelta a)
             -> R.Solution ('ADelta a)
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

  areAllDeltaEmpty :: R.Solution ('ADelta a) -> Bool
  areAllDeltaEmpty =
      R.isEmpty
    . R.filter (\(R.Relation p ts) -> decor p == Delta && (not . T.isEmpty) ts)

  step :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  step = fmap axeDeltaRedundancies
     <$> execClauses (clauses deltaPr) . updateFromDelta . shiftPrevs . elimDecor PrevX2

execClauses :: Eq (PredicateAnn a)
            => [ Clause a ] -> R.Solution a -> IO (R.Solution a)
execClauses clss edb = foldr R.add edb <$> mapM (execClause edb) clss

execClause :: forall a. Eq (PredicateAnn a)
           => R.Solution a -> Clause a -> IO (R.Relation a)
execClause edb Clause{..} = deriveHead <$> foldrM walkBody [ U.empty ] body
  where
  deriveHead :: [ U.Unifier ] -> R.Relation a
  deriveHead unifiers
    | Literal{predicate = pred, terms = terms} <- head =
      let preTuples = map (`U.substitute` terms) unifiers
      in R.Relation pred . T.fromList $ flip map preTuples $ \preTuple ->
        fromMaybe (panic "Range-restriction is violated.")
                  (extractTuples preTuple)

  walkBody :: Literal a -> [ U.Unifier ] -> IO [ U.Unifier ]
  walkBody lit unifiers = fmap (catMaybes . concat) $ sequence $ do
    unifier <- unifiers
    return $ fmap (`U.extend` unifier)
         <$> execLiteral edb (unifier `U.substitute` lit)

execLiteral :: Eq (PredicateAnn a)
            => R.Solution a -> Literal a -> IO [ U.Unifier ]
execLiteral edb Literal{predicate = p@Predicate{nature = nature}, ..}
  | Extralogical foreignAction <- nature = do
    eTuples <- foreignAction terms
    case eTuples of
      Right tuples -> return $ handleTuples terms tuples
      Left msg -> panic $ "Fatal foreign function error: " <> msg
  | otherwise = return . handleTuples terms . T.toList $ R.findTuples edb p
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
