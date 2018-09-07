{-# LANGUAGE FlexibleContexts #-}

module Language.Exalog.Solver
  ( solve
  ) where

import Protolude

import           Language.Exalog.Core
import           Language.Exalog.Stratification (stratify)
import           Language.Exalog.SemiNaive (semiNaive)
import qualified Language.Exalog.Relation as R

solve :: (Eq (PredicateAnn a), Show (ProgramAnn a), Show (PredicateAnn a), Show (ClauseAnn a), Show (LiteralAnn a)) => Program a -> R.Solution a -> IO (R.Solution a)
solve pr edb =
  case eprs of
    Left msg -> panic msg
    Right prs -> foldrM (flip semiNaive) edb (reverse prs)
  where
  eprs = stratify . decorate $ pr
