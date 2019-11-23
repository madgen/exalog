{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Exalog.Annotation
  ( AnnType(..)
  , PredicateAnn(..)
  , LiteralAnn(..)
  , ClauseAnn(..)
  , ProgramAnn(..)
  , KnowledgeAnn(..)
  , type Ann
  , PeelableAnn(..)
  , DecorableAnn(..)
  , SpannableAnn(..)
  , IdentifiableAnn(..)
  , Identifiable
  ) where

import Protolude

import Language.Exalog.SrcLoc
import Language.Exalog.Pretty.Helper (Pretty)

data AnnType =
    ABase
  | ADelta AnnType
  | ADependency AnnType
  | AAdornment AnnType
  | ARename AnnType
  | AProvenance AnnType

data family PredicateAnn (a :: AnnType)
data instance PredicateAnn 'ABase = PredABase {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family LiteralAnn (a :: AnnType)
data instance LiteralAnn 'ABase   = LitABase  {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family ClauseAnn  (a :: AnnType)
data instance ClauseAnn 'ABase    = ClABase   {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family ProgramAnn (a :: AnnType)
data instance ProgramAnn 'ABase   = ProgABase {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family KnowledgeAnn (a :: AnnType)
data instance KnowledgeAnn 'ABase = KnowABase {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

type family Ann (a :: AnnType -> Type) :: (AnnType -> Type)

class PeelableAnn (f :: AnnType -> Type) (ann :: AnnType -> AnnType) where
  peelA :: f (ann a) -> f a

class DecorableAnn (f :: AnnType -> Type) (ann :: AnnType -> AnnType) where
  decorA :: f a -> f (ann a)

class SpannableAnn a where
  annSpan :: a -> SrcSpan

instance SpannableAnn (PredicateAnn 'ABase) where annSpan = span
instance SpannableAnn (LiteralAnn   'ABase) where annSpan = span
instance SpannableAnn (ClauseAnn    'ABase) where annSpan = span
instance SpannableAnn (ProgramAnn   'ABase) where annSpan = span
instance SpannableAnn (KnowledgeAnn 'ABase) where annSpan = span

class IdentifiableAnn a b | a -> b where
  idFragment :: a -> b

instance IdentifiableAnn (PredicateAnn 'ABase) () where idFragment = const ()
instance IdentifiableAnn (LiteralAnn   'ABase) () where idFragment = const ()
instance IdentifiableAnn (ClauseAnn    'ABase) () where idFragment = const ()
instance IdentifiableAnn (ProgramAnn   'ABase) () where idFragment = const ()
instance IdentifiableAnn (KnowledgeAnn 'ABase) () where idFragment = const ()

type Identifiable a b = (IdentifiableAnn a b, Eq b, Ord b, Pretty b)
