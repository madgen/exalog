{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Solver
  ( solve
  , addFact
  , addRule
  , compute
  , evalSolver
  ) where

import Protolude

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Stratification (stratify)
import           Language.Exalog.SemiNaive ( semiNaive
                                           , SemiNaive
                                           , evalSemiNaiveT
                                           )
import qualified Language.Exalog.Relation as R

data SolverSt ann = SolverSt
    { program :: Program ann
    , initEDB :: R.Solution ann
    }

type Solver ann = StateT (SolverSt ann) (SemiNaive ann)

solve :: (SpannableAST a, Eq (PredicateAnn a))
      => Program a -> R.Solution a -> Logger (R.Solution a)
solve = evalSolver compute

evalSolver :: Eq (PredicateAnn ann)
           => Solver ann a -> Program ann -> R.Solution ann -> Logger a
evalSolver action pr = evalSemiNaiveT (evalStateT action (SolverSt pr mempty))

addFact :: Eq (PredicateAnn a) => R.Relation a -> Solver a ()
addFact fact = modify $
  \SolverSt{..} ->
    SolverSt{initEDB = R.add fact initEDB, ..}

addRule :: Clause a -> Solver a ()
addRule cl = modify $
  \SolverSt{program = Program{..}, ..} ->
    SolverSt{program = Program{clauses = cl : clauses, ..}, ..}

compute :: (SpannableAST a, Eq (PredicateAnn a)) => Solver a (R.Solution a)
compute = do
  pr <- program <$> get
  prs <- lift $ lift $ stratify . decorate $ pr
  edb <- lift $ do
    initEDB <- ask
    foldlM (\edb -> local (const edb) . semiNaive) initEDB prs
  -- Filter out non-queries solutions
  return $
    R.filter (\(R.Relation p _) -> PredicateBox p `elem` queryPreds pr) edb
