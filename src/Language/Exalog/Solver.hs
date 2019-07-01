{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.Solver
  ( solve
  , compute
  , evalSolver
  ) where

import Protolude

import Data.List (partition)

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Delta ( mkDeltaStratum
                                       , mkDeltaSolution
                                       , cleanDeltaSolution
                                       )
import           Language.Exalog.SemiNaive ( SemiNaive
                                           , evalSemiNaiveT
                                           , semiNaive
                                           , evalClauses
                                           )
import qualified Language.Exalog.Relation as R

data SolverSt ann = SolverSt
    { program :: Program ann
    , initEDB :: R.Solution ann
    }

type Solver ann = StateT (SolverSt ann) (SemiNaive ann)

solve :: (SpannableAST a, Identifiable (PredicateAnn a) b)
      => Program a -> R.Solution a -> Logger (R.Solution a)
solve = evalSolver compute

evalSolver :: Identifiable (PredicateAnn ann) b
           => Solver ann a -> Program ann -> R.Solution ann -> Logger a
evalSolver action pr sol = evalSemiNaiveT (evalStateT action (SolverSt pr sol)) mempty

compute :: SpannableAST a => Identifiable (PredicateAnn a) b
        => Solver a (R.Solution a)
compute = do
  pr      <- program <$> get
  initEDB <- initEDB <$> get
  let strat = strata pr

  finalEDB <- lift $
    foldM (\edb -> local (const edb) . evalStratum) initEDB strat

  -- Filter out non-query solutions
  let qPreds = queryPreds pr
  pure $ R.filter (\(R.Relation p _) -> PredicateBox p `elem` qPreds) finalEDB

evalStratum :: forall a b. SpannableAST a => Identifiable (PredicateAnn a) b
            => [ Clause a ] -> SemiNaive a (R.Solution a)
evalStratum stratum = do
  simpleEDB <- evalClauses simpleClss

  deltaEDB  <- local (const simpleEDB)
             $ withDifferentEnvironment envMap
             $ semiNaive deltaClss

  pure $ simpleEDB <> cleanDeltaSolution deltaEDB
  where
  (simpleClss, complexClss) = partitionBySimplicity stratum
  deltaClss                 = mkDeltaStratum complexClss

  envMap = mkDeltaSolution (intentionals complexClss)

  intentionalPreds = intentionals stratum

  partitionBySimplicity :: [ Clause a ] -> ([ Clause a ], [ Clause a ])
  partitionBySimplicity =
    partition (all ((`notElem` intentionalPreds) . predicateBox) . body)

withDifferentEnvironment :: Monad m
                         => (r -> s) -> ReaderT s m a -> ReaderT r m a
withDifferentEnvironment envMap (ReaderT f) =
  ReaderT $ f . envMap
