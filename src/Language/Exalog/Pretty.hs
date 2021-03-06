{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.Pretty
  ( pp
  ) where

import Protolude hiding ((<>), empty, head, pred)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V

import Text.PrettyPrint

import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
import           Language.Exalog.Pretty.Helper

-- Core pretty instances

instance Pretty PredicateSymbol where
  pretty (PredicateSymbol predSym) = pretty predSym

instance ( IdentifiableAnn (Ann (Predicate n) ann) id
         , Pretty id
         )
    => Pretty (Predicate n ann) where
  pretty Predicate{..} =
      pretty _predSym
   <> "_" <>
      case _nature of
        Logical{}    -> char 'L'
        Extralogical{} -> char 'E'
   <> ("PA" <> colon) <?> pretty (idFragment _annotation)

instance Pretty Sym where
  pretty (SymText   t) = doubleQuotes $ pretty t
  pretty (SymInt    i) = int i
  pretty (SymBool   b) = pretty b

instance Pretty Var where
  pretty (Var v) = char '\'' <> pretty v

instance Pretty Term where
  pretty (TSym s) = pretty s
  pretty (TVar v) = pretty v
  pretty TWild    = "_"

instance ( IdentifiableAnn (PredicateAnn ann) id
         , Pretty id
         ) => Pretty (PredicateBox ann) where
  pretty (PredicateBox p) = pretty p

instance ( IdentifiableAnn (Ann (Predicate n) ann) id
         , IdentifiableAnn (Ann Literal ann) id'
         , Pretty id
         , Pretty id'
         ) => Pretty (Literal ann) where
  pretty Literal{..} =
       cond (_polarity == Negative) (text "not" <> space)
   <+> pretty _predicate
   <> ("LA" <> colon) <?> pretty (idFragment _annotation)
   <> (parens . csep . prettyC $ _terms)

instance Pretty (Literal ann) => Pretty (Clause ann) where
  pretty Clause{..} =
    pretty _head <+> ":-" <+> (csep . prettyC $ _body) <> "."

instance Pretty (Clause ann) => Pretty (Stratum ann) where
  pretty (Stratum cls) = vcat $ prettyC cls

instance Pretty (Stratum ann) => Pretty (Program ann) where
  pretty Program{..} = vcat . punctuate "\n"
                     $ prettyStratum <$> zip [(0 :: Int)..] _strata
    where
    prettyStratum (i, stratum) =
      vcat [ "Stratum #" <> pretty i <> ":", pretty stratum ]

-- Annotation instances

instance Pretty (PredicateAnn 'ABase) where pretty _ = empty
instance Pretty (LiteralAnn   'ABase) where pretty _ = empty
instance Pretty (ClauseAnn    'ABase) where pretty _ = empty
instance Pretty (ProgramAnn   'ABase) where pretty _ = empty
instance Pretty (KnowledgeAnn 'ABase) where pretty _ = empty

-- Knowledge base related instances

instance
  ( Pretty (KnowledgeAnn ann)
  , Identifiable (KnowledgeAnn ann) id
  , Identifiable (PredicateAnn ann) id'
  ) => Pretty (KB.Knowledge ann) where
  pretty (KB.Knowledge ann pred syms) =
    pretty pred <> pretty ann <> (csep . prettyC) syms

instance
  ( Pretty (KnowledgeAnn ann)
  , Identifiable (KnowledgeAnn ann) id
  , Identifiable (PredicateAnn ann) id'
  ) => Pretty (KB.Set ann) where
  pretty = vcat . map pretty . KB.toList

-- Common pretty instances

instance Pretty Bool where
  pretty True  = "true"
  pretty False = "false"

-- Collections

instance Pretty a => PrettyCollection (NE.NonEmpty a) where
  prettyC = map pretty . NE.toList

instance Pretty a => PrettyCollection (V.Vector n a) where
  prettyC = map pretty . V.toList
