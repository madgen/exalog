{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Solver
  ( solve
  , addFact
  , addRule
  , compute
  , facts
  ) where

import Protolude

import Control.Monad.Trans.State (StateT)

import           Language.Exalog.Core
import           Language.Exalog.Stratification (stratify)
import           Language.Exalog.SemiNaive (semiNaive, SemiNaiveM)
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

type SolverM ann = StateT (Program ann) (SemiNaiveM ann)

solve :: Eq (PredicateAnn a) => Program a -> R.Solution a -> IO (R.Solution a)
solve pr = execStateT (execStateT compute pr)

addFact :: Eq (PredicateAnn a) => R.Relation a -> SolverM a ()
addFact rel = lift $ modify (R.add rel)

addRule :: Clause a -> SolverM a ()
addRule cl = modify $ \Program{..} -> Program{clauses = cl : clauses, ..}

compute :: Eq (PredicateAnn a) => SolverM a ()
compute = do
  eprs <- stratify . decorate <$> get
  case eprs of
    Left msg -> panic msg
    Right prs -> lift $ traverse_ semiNaive prs

facts :: Eq (PredicateAnn a) => Predicate n a -> SolverM a (T.Tuples n)
facts p = R.findTuples p <$> lift get
