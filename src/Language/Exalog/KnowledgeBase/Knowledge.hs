{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.KnowledgeBase.Knowledge where

import Protolude hiding (pred)

import Data.Singletons
import Data.Singletons.Decide (Decision(..))

import qualified Data.Vector.Sized as V
import Data.Type.Equality ((:~:)(..))

import Language.Exalog.Core

data Knowledge a = forall n. Knowledge 
  { _annotation :: (KnowledgeAnn a)
  , _predicate  :: (Predicate n a)
  , _terms      :: (V.Vector n Sym)
  }

class KnowledgeMaker ann where
  mkKnowledge :: Predicate n ann -> V.Vector n Sym -> Knowledge ann

instance KnowledgeMaker ABase where
  mkKnowledge pred syms = Knowledge KnowABase pred syms

deriving instance (Show (KnowledgeAnn ann), Show (PredicateAnn ann)) => Show (Knowledge ann)

instance
  ( IdentifiableAnn (PredicateAnn a) b
  , IdentifiableAnn (KnowledgeAnn a) c
  , Ord b
  , Ord c
  ) => Ord (Knowledge a) where
  Knowledge{_annotation = ann, _predicate = pred, _terms = terms} `compare`
    Knowledge{_annotation = ann', _predicate = pred', _terms = terms'}
    | Proved Refl <- sameArity pred pred' = (idFragment ann, pred, terms) `compare` (idFragment ann', pred', terms')
    | otherwise = fromSing (_arity pred) `compare` fromSing (_arity pred')

instance
  ( IdentifiableAnn (PredicateAnn a) b
  , IdentifiableAnn (KnowledgeAnn a) c
  , Eq b
  , Eq c
  ) => Eq (Knowledge a) where
  Knowledge{_annotation = ann, _predicate = pred, _terms = terms} == 
    Knowledge{_annotation = ann', _predicate = pred', _terms = terms'}
    | Proved Refl <- pred `sameArity` pred' =
      idFragment ann == idFragment ann' &&
      pred == pred' &&
      terms == terms'
    | otherwise = False