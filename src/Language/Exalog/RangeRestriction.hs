{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.RangeRestriction (RangeRestriction(..)) where

import Protolude hiding (diff, head)

import           Data.List ((\\))
import qualified Data.List.NonEmpty as NE

import Language.Exalog.Core
import Language.Exalog.Logger
import Language.Exalog.Pretty
import Language.Exalog.SrcLoc (Spannable(..))

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
  checkRangeRestriction :: ast -> Logger ()
  isRangeRestricted     :: ast -> Bool

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Program ann) where
  checkRangeRestriction Program{..} = traverse_ checkRangeRestriction clauses
  isRangeRestricted Program{..} = all isRangeRestricted clauses

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Clause ann) where
  checkRangeRestriction cl =
    unless (isRangeRestricted cl) $
      scold (Just $ span cl) $ "Range restriction is violated."

  isRangeRestricted Clause{..} =
    null $ variables head \\ mconcat (variables <$> NE.toList body)
