{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.SemiNaive where

import Protolude hiding (head, pred)

import           Data.Function (id)
import           Data.List (lookup)
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
    rels <- runClauses simpleClss edb
    let deltaSol = mkDeltaSolution revPr . foldr R.add edb $ rels
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

  runDelta :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  runDelta edb = axeDeltaRedundancies
               . foldr R.add edb
             <$> runClauses (clauses deltaPr) edb

  axeDeltaRedundancies :: R.Solution ('ADelta a)
                       -> R.Solution ('ADelta a)
  axeDeltaRedundancies edb = (`R.atEach` edb) $ \(p, ts) ->
    case decor p of
      Delta -> ts `T.difference` R.findTuples edb (updateDecor Normal p)
      _ -> ts

  areAllDeltaEmpty :: R.Solution ('ADelta a) -> Bool
  areAllDeltaEmpty =
      R.isEmpty
    . R.filter (\(R.Relation p ts) -> decor p == Delta && (not . T.isEmpty) ts)

  step :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  step = runDelta . updateFromDelta . shiftPrevs . elimDecor PrevX2

runClauses :: Eq (PredicateAnn a)
           => [ Clause a ] -> R.Solution a -> IO [ R.Relation a ]
runClauses clss edb = mapM (execClause edb) clss

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
  | Extralogical action <- nature = either panic _ <$> action terms
  | otherwise = do
    let unifiers = mapMaybe (U.unify terms) . T.toList $ R.findTuples edb p
    return $ case polarity of
      Positive -> unifiers
      Negative -> [ U.empty | null unifiers ]

extractTuples :: V.Vector n Term -> Maybe (V.Vector n Sym)
extractTuples = traverse (\case
  TSym sym -> Just sym
  TVar{}   -> Nothing)

reverseClauses :: Program a -> Program a
reverseClauses pr = pr { clauses = map reverseClause . clauses $ pr}
  where
  reverseClause :: Clause a -> Clause a
  reverseClause cl = cl { body = NE.reverse . body $ cl }
