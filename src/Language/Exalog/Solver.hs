{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Solver
  ( solve
  , addFact
  , addRule
  , compute
  , evalSolverM
  ) where

import Protolude

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Stratification (stratify)
import           Language.Exalog.SemiNaive ( semiNaive
                                           , SemiNaiveM
                                           , evalSemiNaiveMT
                                           )
import qualified Language.Exalog.Relation as R

data SolverSt ann = SolverSt
    { program :: Program ann
    , initEDB :: R.Solution ann
    }

type SolverM ann = StateT (SolverSt ann) (SemiNaiveM ann)

solve :: Eq (PredicateAnn a)
      => Program a -> R.Solution a -> LoggerM (R.Solution a)
solve = evalSolverM compute

evalSolverM :: Eq (PredicateAnn ann)
            => SolverM ann a -> Program ann -> R.Solution ann -> LoggerM a
evalSolverM action pr = evalSemiNaiveMT (evalStateT action (SolverSt pr mempty))

addFact :: Eq (PredicateAnn a) => R.Relation a -> SolverM a ()
addFact fact = modify $
  \SolverSt{..} ->
    SolverSt{initEDB = R.add fact initEDB, ..}

addRule :: Clause a -> SolverM a ()
addRule cl = modify $
  \SolverSt{program = Program{..}, ..} ->
    SolverSt{program = Program{clauses = cl : clauses, ..}, ..}

compute :: Eq (PredicateAnn a) => SolverM a (R.Solution a)
compute = do
  pr <- program <$> get
  let eprs = stratify . decorate $ pr
  edb <- case eprs of
    Left msg -> panic msg
    Right prs -> lift $ do
      initEDB <- ask
      foldlM (\edb -> local (const edb) . semiNaive) initEDB prs
  -- Filter out non-queries solutions
  return $
    R.filter (\(R.Relation p _) -> PredicateBox p `elem` queryPreds pr) edb
