{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Exalog.Annotation
  ( AnnType(..)
  , type PredicateAnn
  , type LiteralAnn
  , type ClauseAnn
  , type ProgramAnn
  , type Ann
  , PeelableAnn(..)
  , DecorableAnn(..)
  ) where

import Data.Kind

data AnnType = ABase | ADelta AnnType | ADependency AnnType

data family PredicateAnn (a :: AnnType)
newtype instance PredicateAnn 'ABase = PredABase ()

data family LiteralAnn (a :: AnnType)
newtype instance LiteralAnn   'ABase = LitABase ()

data family ClauseAnn  (a :: AnnType)
newtype instance ClauseAnn    'ABase = ClABase ()

data family ProgramAnn (a :: AnnType)
newtype instance ProgramAnn   'ABase = ProgABase ()

type family Ann (a :: AnnType -> Type) :: (AnnType -> Type)

class PeelableAnn (f :: AnnType -> Type) (ann :: AnnType -> AnnType) where
  peelA :: f (ann a) -> f a

class DecorableAnn (f :: AnnType -> Type) (ann :: AnnType -> AnnType) where
  decorA :: f a -> f (ann a)
