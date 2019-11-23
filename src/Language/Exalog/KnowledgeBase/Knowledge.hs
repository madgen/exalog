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

data Knowledge a = forall n. Knowledge 
  { _kannotation :: (KnowledgeAnn a)
  , _kpredicate  :: (Predicate n a)
  , _kterms      :: (V.Vector n Sym)
  }

deriving instance (Show (KnowledgeAnn ann), Show (PredicateAnn ann)) => Show (Knowledge ann)

instance (
  IdentifiableAnn (PredicateAnn a) b,
  IdentifiableAnn (KnowledgeAnn a) c,
  Ord b,
  Ord c
  ) => Ord (Knowledge a) where
  know@Knowledge{_kannotation = ann, _kpredicate = pred, _kterms = terms} `compare`
    know'@Knowledge{_kannotation = ann', _kpredicate = pred', _kterms = terms'}
    | Proved Refl <- sameArity pred pred' = (idFragment ann, pred, terms) `compare` (idFragment ann', pred', terms')
    | otherwise = fromSing (_arity pred) `compare` fromSing (_arity pred')

instance (
  IdentifiableAnn (PredicateAnn a) b,
  IdentifiableAnn (KnowledgeAnn a) c,
  Eq b,
  Eq c
  ) => Eq (Knowledge a) where
  know@Knowledge{_kannotation = ann, _kpredicate = pred, _kterms = terms} == 
    know'@Knowledge{_kannotation = ann', _kpredicate = pred', _kterms = terms'}
    | Proved Refl <- pred `sameArity` pred' =
      idFragment ann == idFragment ann' &&
      pred == pred' &&
      terms == terms'
    | otherwise = False
