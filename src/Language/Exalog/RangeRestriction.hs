{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.RangeRestriction
  ( RangeRestriction(..)
  , fixRangeRestriction
  ) where

import Protolude hiding (diff, head, pred, sym)

import Control.Arrow ((&&&))

import           Data.List ((\\))
import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Dataflow
import           Language.Exalog.DataflowRepair
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SrcLoc (Spannable(..))

-- |Checks if all variables in the head appear in the bodies of the
-- clauses.
--
-- This allows domain independence (changing the underlying types in a way
-- compatible with the data in the database/EDB) does not cause a change in
-- the query results.
--
-- It also contributes to finiteness of results (allowing tabulation of all
-- ground facts) by preventing concluding facts such as $p(X,X)$.
--
-- A final benefit is facilitating data provenance. This restriction does
-- not solely ensure but contribute to the fact that all ground terms we
-- use come from somewhere in the original EDB.
class RangeRestriction ast where
  checkRangeRestriction      :: ast -> Logger ()
  isRangeRestricted          :: ast -> Bool

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Program ann) where
  checkRangeRestriction Program{..} = traverse_ checkRangeRestriction (join $ map _unStratum _strata)
  isRangeRestricted Program{..} = all isRangeRestricted (join $ map _unStratum _strata)

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Clause ann) where
  checkRangeRestriction cl =
    unless (isRangeRestricted cl) $
      scold (Just $ span cl) "Range restriction is violated."

  isRangeRestricted Clause{..} =
    null $ variables _head \\ mconcat (variables <$> NE.toList _body)

fixRangeRestriction :: (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
                    -> Logger (Program 'ABase, R.Solution 'ABase)
fixRangeRestriction =
  fixDataflow (pure <$> rangeRestrictionViolations)
              "Not range-restricted and cannot be repaired due to its dataflow."

rangeRestrictionViolations :: Clause ('ARename ann) -> [ (FlowSink ann, Var) ]
rangeRestrictionViolations Clause{..} = map (genSink . fst &&& snd)
                                      . filter (isRestriction . snd)
                                      . zip [0..]
                                      $ variables _head
  where
  isRestriction var = var `notElem` bodyVariables
  genSink = FSinkPredicate (predicateBox _head)
  bodyVariables = mconcat $ variables <$> NE.toList _body
