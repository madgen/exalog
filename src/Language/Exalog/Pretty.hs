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
import           Data.String (fromString)
import           Data.Text (unpack)
import qualified Data.Vector.Sized as V

import Text.PrettyPrint

import           Language.Exalog.Core
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T

pp :: Pretty a => a -> Text
pp = fromString . render . pretty

class Pretty a where
  pretty :: a -> Doc

class PrettyCollection a where
  prettyC :: a -> [ Doc ]

infixl 7 <?>
infix  7 <+?>

(<?>) :: Doc -> Doc -> Doc
d1 <?> d2
  | isEmpty d1 || isEmpty d2 = empty
  | otherwise = d1 <> d2

(<+?>) :: Doc -> Doc -> Doc
d1 <+?> d2
  | isEmpty d1 || isEmpty d2 = empty
  | otherwise = d1 <+> d2

cond :: Bool -> Doc -> Doc
cond True doc = doc
cond False _  = empty

-- | Comma separate Docs
csep :: [ Doc ] -> Doc
csep = hsep . punctuate comma

-- Core pretty instances

instance Pretty (Ann (Predicate n) ann) => Pretty (Predicate n ann) where
  pretty Predicate{..} =
      pretty fxSym
   <> "_" <>
      case nature of
        Logical{}    -> char 'L'
        Extralogical{} -> char 'E'
   <> ("PA" <> colon) <?> pretty annotation

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

instance ( Pretty (PredicateAnn ann)
         , Pretty (Ann Literal ann)
         ) => Pretty (Literal ann) where
  pretty Literal{..} =
       cond (polarity == Negative) (text "not" <> space)
   <+> pretty predicate
   <> ("LA" <> colon) <?> pretty annotation
   <> (parens . csep . prettyC $ terms)

instance ( Pretty (PredicateAnn ann)
         , Pretty (Ann Literal ann)
         , Pretty (Ann Clause ann)
         ) => Pretty (Clause ann) where
  pretty Clause{..} =
    pretty head <+> ":-" <+> (csep . prettyC $ body) <> "."

instance ( Pretty (PredicateAnn ann)
         , Pretty (Ann Literal ann)
         , Pretty (Ann Clause ann)
         , Pretty (Ann Program ann)
         ) => Pretty (Program ann) where
  pretty Program{..} = vcat . prettyC $ clauses

-- Annotation instances

instance Pretty (PredicateAnn 'ABase) where pretty _ = empty
instance Pretty (LiteralAnn   'ABase) where pretty _ = empty
instance Pretty (ClauseAnn    'ABase) where pretty _ = empty
instance Pretty (ProgramAnn   'ABase) where pretty _ = empty

-- Solution related data type instances

instance Pretty (PredicateAnn ann) => Pretty (R.Relation ann) where
  pretty (R.Relation predicate tuples) =
    pretty predicate <+> parens (int . T.size $ tuples) <+> "= {"
    $+$ (nest 2 . vcat . map csep . groupsOf 8 . prettyC $ tuples)
    $+$ rbrace
    where
    groupsOf :: Int -> [ a ] -> [ [ a ] ]
    groupsOf _ [] = []
    groupsOf i xs = let (gr, rest) = splitAt i xs in gr : groupsOf i rest

instance Pretty (PredicateAnn ann) => Pretty (R.Solution ann) where
  pretty = vcat . prettyC . R.toList

-- Common pretty instances

instance Pretty Text where
  pretty = text . unpack

instance Pretty Bool where
  pretty True  = "true"
  pretty False = "false"

-- Collections

instance Pretty a => PrettyCollection [ a ] where
  prettyC = map pretty

instance Pretty a => PrettyCollection (NE.NonEmpty a) where
  prettyC = map pretty . NE.toList

instance Pretty a => PrettyCollection (V.Vector n a) where
  prettyC = map pretty . V.toList

instance PrettyCollection (T.Tuples n) where
  prettyC = map (parens . csep . prettyC) . T.toList
