{-# LANGUAGE MonoLocalBinds #-}
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
    { _program :: Program ann
    , _initEDB :: R.Solution ann
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
  pr      <- _program <$> get
  initEDB <- _initEDB <$> get
  let strat = _strata pr

  finalEDB <- lift $
    foldM (\edb -> local (const edb) . evalStratum) initEDB strat

  -- Filter out non-query solutions
  let qPreds = _queries pr
  pure $ R.filter (\(R.Relation p _) -> PredicateBox p `elem` qPreds) finalEDB

evalStratum :: forall a b. SpannableAST a => Identifiable (PredicateAnn a) b
            => Stratum a -> SemiNaive a (R.Solution a)
evalStratum stratum@(Stratum cls) = do
  simpleEDB <-
    if null simpleClauses
      then ask
      else evalClauses simpleClauses

  local (const simpleEDB) $
    if null (_unStratum complexStratum)
      then ask
      else cleanDeltaSolution <$>
        (withDifferentEnvironment envMap
         . semiNaive
         $ deltaStratum)
  where
  (simpleClauses, complexStratum) = second Stratum $ partitionBySimplicity cls
  deltaStratum                    = mkDeltaStratum complexStratum

  envMap = mkDeltaSolution (intentionals complexStratum)

  intentionalPreds = intentionals stratum

  partitionBySimplicity :: [ Clause a ] -> ([ Clause a ], [ Clause a ])
  partitionBySimplicity =
    partition (all ((`notElem` intentionalPreds) . predicateBox) . _body)

withDifferentEnvironment :: Monad m
                         => (r -> s) -> ReaderT s m a -> ReaderT r m a
withDifferentEnvironment envMap (ReaderT f) =
  ReaderT $ f . envMap
