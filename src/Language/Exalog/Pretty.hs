{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.Pretty
  ( pp
  ) where

import Protolude hiding ((<>), empty, head)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V

import Text.PrettyPrint

import           Language.Exalog.Core
import           Language.Exalog.Pretty.Helper
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

-- Core pretty instances

instance Identifiable (Ann (Predicate n) ann) b
    => Pretty (Predicate n ann) where
  pretty Predicate{..} =
      pretty fxSym
   <> "_" <>
      case nature of
        Logical{}    -> char 'L'
        Extralogical{} -> char 'E'
   <> ("PA" <> colon) <?> pretty (idFragment annotation)

instance Pretty Sym where
  pretty (SymText   t) = doubleQuotes $ pretty t
  pretty (SymInt    i) = int i
  pretty (SymDouble d) = double d
  pretty (SymBool   b) = pretty b

instance Pretty Var where
  pretty (Var v) = char '\'' <> pretty v

instance Pretty Term where
  pretty (TSym s) = pretty s
  pretty (TVar v) = pretty v

instance ( Identifiable (PredicateAnn ann) a
         , Identifiable (Ann Literal ann) b
         ) => Pretty (Literal ann) where
  pretty Literal{..} =
       cond (polarity == Negative) (text "not" <> space)
   <+> pretty predicate
   <> ("LA" <> colon) <?> pretty (idFragment annotation)
   <> (parens . csep . prettyC $ terms)

instance ( Identifiable (PredicateAnn ann) a
         , Identifiable (Ann Literal ann) b
         ) => Pretty (Clause ann) where
  pretty Clause{..} =
    pretty head <+> ":-" <+> (csep . prettyC $ body) <> "."

instance ( Identifiable (PredicateAnn ann) a
         , Identifiable (Ann Literal ann) b
         , Identifiable (Ann Clause ann) c
         ) => Pretty (Program ann) where
  pretty Program{..} = vcat . prettyC $ clauses

-- Annotation instances

instance Pretty (PredicateAnn 'ABase) where pretty _ = empty
instance Pretty (LiteralAnn   'ABase) where pretty _ = empty
instance Pretty (ClauseAnn    'ABase) where pretty _ = empty
instance Pretty (ProgramAnn   'ABase) where pretty _ = empty

-- Solution related data type instances

instance Identifiable (PredicateAnn ann) b => Pretty (R.Relation ann) where
  pretty (R.Relation predicate tuples) =
    pretty predicate <+> parens (int . T.size $ tuples) <+> "= {"
    $+$ (nest 2 . vcat . map csep . groupsOf 8 . prettyC $ tuples)
    $+$ rbrace
    where
    groupsOf :: Int -> [ a ] -> [ [ a ] ]
    groupsOf _ [] = []
    groupsOf i xs = let (gr, rest) = splitAt i xs in gr : groupsOf i rest

instance Identifiable (PredicateAnn ann) b => Pretty (R.Solution ann) where
  pretty = vcat . prettyC . R.toList

-- Common pretty instances

instance Pretty Bool where
  pretty True  = "true"
  pretty False = "false"

-- Collections

instance Pretty a => PrettyCollection (NE.NonEmpty a) where
  prettyC = map pretty . NE.toList

instance Pretty a => PrettyCollection (V.Vector n a) where
  prettyC = map pretty . V.toList

instance PrettyCollection (T.Tuples n) where
  prettyC = map (parens . csep . prettyC) . T.toList
