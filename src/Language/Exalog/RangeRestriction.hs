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

import           Data.List ((\\), unzip3)
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
  checkRangeRestriction Program{..} = traverse_ checkRangeRestriction clauses
  isRangeRestricted Program{..} = all isRangeRestricted clauses

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Clause ann) where
  checkRangeRestriction cl =
    unless (isRangeRestricted cl) $
      scold (Just $ span cl) "Range restriction is violated."

  isRangeRestricted Clause{..} =
    null $ variables head \\ mconcat (variables <$> NE.toList body)

fixRangeRestriction :: (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
                    -> Logger (Program 'ABase, R.Solution 'ABase)
fixRangeRestriction (pr@Program{..}, sol) = runRepairT pr $ do
  (originalClauses, guardClausess, guardSols) <- unzip3 <$>
    traverse fixRangeRestrictionClause clauses

  pure ( Program
          { annotation = peelA annotation
          , clauses    = originalClauses <> join guardClausess
          , queryPreds = (PredicateBox . peel $$) <$> queryPreds
          , ..}
       , mconcat (R.rename peel sol : guardSols)
       )

fixRangeRestrictionClause :: Clause ('ARename 'ABase)
                          -> RepairT Logger
                             ( Clause 'ABase
                             , [ Clause 'ABase ]
                             , R.Solution 'ABase
                             )
fixRangeRestrictionClause cl@Clause{..} = do
  mGuard <- sequence <$> traverse (uncurry (mkGuard $ span head)) violations

  case unzip3 <$> mGuard of
    Just (guardLits, guardClausess, guardSols) ->
      pure ( Clause
              { annotation = peelA annotation
              , head       = peel head
              , body       = foldr' NE.cons (peel <$> body) guardLits
              , ..}
           , join guardClausess
           , mconcat guardSols
           )
    Nothing -> lift $ lift $ scold (Just $ span head)
      "Not range-restricted and cannot be repaired due to its dataflow."
  where
  violations = rangeRestrictionViolations cl

rangeRestrictionViolations :: Clause ('ARename ann) -> [ (FlowSink ann, Var) ]
rangeRestrictionViolations Clause{..} = map (genSink . fst &&& snd)
                                      . filter (isRestriction . snd)
                                      . zip [0..]
                                      $ variables head
  where
  isRestriction var = var `notElem` bodyVariables
  genSink = FSinkPredicate (predicateBox head)
  bodyVariables = mconcat $ variables <$> NE.toList body
