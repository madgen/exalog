{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Exalog.KnowledgeBase.Knowledge where

import Protolude hiding (pred)

import GHC.Show (Show)

import Data.Singletons
import Data.Singletons.Decide (Decision(..))

import qualified Data.Vector.Sized as V
import Data.Type.Equality ((:~:)(..))

import Language.Exalog.Core

data Knowledge a = forall n. Knowledge (Predicate n a) (V.Vector n Sym)

deriving instance Show (PredicateAnn ann) => Show (Knowledge ann)

instance (IdentifiableAnn (PredicateAnn a) b, Ord b) => Ord (Knowledge a) where
  Knowledge pred terms `compare` Knowledge pred' terms'
    | Proved Refl <- sameArity pred pred' = (pred,terms) `compare` (pred',terms')
    | otherwise = fromSing (_arity pred) `compare` fromSing (_arity pred')

instance (IdentifiableAnn (PredicateAnn a) b, Eq b) => Eq (Knowledge a) where
  Knowledge pred terms == Knowledge pred' terms'
    | Proved Refl <- pred `sameArity` pred' = pred == pred' && terms == terms'
    | otherwise = False

