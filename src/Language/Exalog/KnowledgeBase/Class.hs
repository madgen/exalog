{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.KnowledgeBase.Class
  ( Knowledgeable(..)
  , map
  ) where

import qualified Protolude as P
import           Protolude hiding (pred, filter, toList, map)

import qualified Data.Vector.Sized as V

import Language.Exalog.Core

import Language.Exalog.KnowledgeBase.Knowledge

class Knowledgeable sol a where
  fromList :: [ Knowledge a ] -> sol a
  toList :: sol a -> [ Knowledge a ]

  add :: Knowledge a -> sol a -> sol a
  partition :: (Knowledge a -> Bool) -> sol a -> (sol a, sol a)
  filter :: (Knowledge a -> Bool) -> sol a -> sol a
  difference :: sol a -> sol a -> sol a
  findByPred :: Predicate n a -> sol a -> [ V.Vector n Sym ]

  atEach :: IdentifiableAnn (PredicateAnn b) id
         => Ord id
         => (Knowledge a -> Knowledge b) -> sol a -> sol b

  singleton :: Knowledge a -> sol a

  size :: sol a -> Int
  null :: sol a -> Bool

map :: Knowledgeable kb a => (Knowledge a -> b) -> kb a -> [ b ]
map f = P.map f . toList
