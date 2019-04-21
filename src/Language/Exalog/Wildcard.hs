{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Exalog.Wildcard (checkWildcards) where

import Protolude

import Language.Exalog.Core
import Language.Exalog.Logger
import Language.Exalog.SrcLoc (Spannable(span))

-- |Clause heads cannot contain wildcards
checkWildcards :: Spannable (Clause ann) => Program ann -> Logger ()
checkWildcards Program{..} = traverse_ checkWildcardsInClause clauses

checkWildcardsInClause :: Spannable (Clause ann) => Clause ann -> Logger ()
checkWildcardsInClause cl@Clause{head = Literal{..}} =
  when (TWild `elem` terms) $
    scold (Just $ span cl) "Clause heads cannot have wildcards."
