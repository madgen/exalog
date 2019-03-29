{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Annotation
  ( AnnType(..)
  , PredicateAnn(..)
  , LiteralAnn(..)
  , ClauseAnn(..)
  , ProgramAnn(..)
  , type Ann
  , PeelableAnn(..)
  , DecorableAnn(..)
  , SpannableAnn(..)
  ) where

import Protolude

import Language.Exalog.SrcLoc

data AnnType = ABase | ADelta AnnType | ADependency AnnType

data family PredicateAnn (a :: AnnType)
data instance PredicateAnn 'ABase = PredABase {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family LiteralAnn (a :: AnnType)
data instance LiteralAnn   'ABase = LitABase  {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family ClauseAnn  (a :: AnnType)
data instance ClauseAnn    'ABase = ClABase   {_span :: SrcSpan}
  deriving (Eq, Ord, Show)

data family ProgramAnn (a :: AnnType)
data instance ProgramAnn   'ABase = ProgABase {_span :: SrcSpan}
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
