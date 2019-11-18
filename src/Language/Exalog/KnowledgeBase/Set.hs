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
         ) => Knowledgeable Identity Set ann where
  fromList = pure . Set . S.fromList

  add x = pure . coerce . S.insert x . coerce
  filter p = pure . coerce . S.filter p . coerce

  atEach f = pure . coerce . S.map f . coerce

  empty = pure . coerce $ S.empty
  singleton = pure . coerce . S.singleton

  size = pure . S.size . coerce
  null = pure . S.null . coerce
