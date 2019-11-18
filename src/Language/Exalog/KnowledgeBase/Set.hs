{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Exalog.KnowledgeBase.Set where

import Protolude hiding (Set)

import GHC.Prim (coerce)

import qualified Data.Set as S

import Language.Exalog.Annotation
import Language.Exalog.KnowledgeBase.Class
import Language.Exalog.KnowledgeBase.Knowledge

newtype Set ann = Set (S.Set (Knowledge ann))

deriving instance (IdentifiableAnn (PredicateAnn ann) id, Eq id) => Eq (Set ann)
deriving instance (IdentifiableAnn (PredicateAnn ann) id, Ord id) => Semigroup (Set ann)
deriving instance (IdentifiableAnn (PredicateAnn ann) id, Ord id) => Monoid (Set ann)

instance ( IdentifiableAnn (PredicateAnn ann) id
         , Ord id
         ) => Knowledgeable Set ann where
  fromList = Set . S.fromList

  add x = coerce . S.insert x . coerce
  filter p = coerce . S.filter p . coerce

  atEach f = coerce . S.map f . coerce

  empty = coerce S.empty
  singleton = coerce . S.singleton

  size = S.size . coerce
  null = S.null . coerce
