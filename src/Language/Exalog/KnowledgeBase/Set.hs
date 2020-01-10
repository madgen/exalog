{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Exalog.KnowledgeBase.Set where

import Protolude hiding (Set, toList, pred)

import GHC.Prim (coerce)

import qualified Data.Set as S
import           Data.Singletons.Decide (Decision(Proved))
import           Data.Type.Equality ((:~:)(Refl))

import Language.Exalog.Core
import Language.Exalog.KnowledgeBase.Class
import Language.Exalog.KnowledgeBase.Knowledge

newtype Set ann = Set (S.Set (Knowledge ann))

deriving instance
  ( IdentifiableAnn (Ann Knowledge ann) id1
  , IdentifiableAnn (PredicateAnn ann)  id2
  , Ord id1, Ord id2
  ) => Eq (Set ann)

deriving instance
  ( IdentifiableAnn (Ann Knowledge ann) id1
  , IdentifiableAnn (PredicateAnn ann)  id2
  , Ord id1, Ord id2
  ) => Semigroup (Set ann)

deriving instance
  ( IdentifiableAnn (Ann Knowledge ann) id1
  , IdentifiableAnn (PredicateAnn ann)  id2
  , Ord id1, Ord id2
  ) => Monoid (Set ann)

deriving instance (Show (KnowledgeAnn ann), Show (PredicateAnn ann)) => Show (Set ann)

instance
  ( IdentifiableAnn (Ann Knowledge ann) id1
  , IdentifiableAnn (Ann (Predicate n) ann) id2
  , Ord id1, Ord id2
  ) => Knowledgeable Set ann where
  fromList = Set . S.fromList
  toList = S.toList . coerce

  add x = coerce . S.insert x . coerce

  partition p = coerce . S.partition p . coerce

  filter p = coerce . S.filter p . coerce

  difference kb kb' = coerce $ S.difference (coerce kb) (coerce kb')

  findByPred pred kb = foldr' go mempty (toList kb)
    where
    go (Knowledge _ pred' syms) acc
      | Proved Refl <- pred `sameArity` pred'
      , pred == pred' = syms : acc
      | otherwise = acc

  atEach f = coerce . S.map f . coerce

  singleton = coerce . S.singleton

  size = S.size . coerce

  null = S.null . coerce
