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
