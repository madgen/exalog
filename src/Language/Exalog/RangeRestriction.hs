{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Exalog.RangeRestriction (checkRangeRestriction) where

import Protolude hiding (diff, head)

import           Data.List ((\\))
import qualified Data.List.NonEmpty as NE

import Language.Exalog.Core
import Language.Exalog.Logger
import Language.Exalog.Pretty
import Language.Exalog.SrcLoc (span)

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
checkRangeRestriction :: SpannableAnn (ClauseAnn ann) => Program ann -> Logger ()
checkRangeRestriction Program{..} = traverse_ checkClause clauses

checkClause :: SpannableAnn (ClauseAnn ann) => Clause ann -> Logger ()
checkClause cl@Clause{..} =
  case diff of
    []        -> pure ()
    (var : _) -> scold (Just $ span cl) $
      "Range restriction is violated " <> pp var <>
      " doesn't occur in the body."
  where
  diff = variables head \\ mconcat (variables <$> NE.toList body)
