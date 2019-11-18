{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.KnowledgeBase.Class
  ( Knowledgeable(..)
  ) where

import Protolude

import Language.Exalog.Annotation
import Language.Exalog.KnowledgeBase.Knowledge

class (Monoid (sol a)) => Knowledgeable sol a where
  fromList :: [ Knowledge a ] -> sol a

  add :: Knowledge a -> sol a -> sol a
  filter :: (Knowledge a -> Bool) -> sol a -> sol a

  atEach :: ( IdentifiableAnn (PredicateAnn b) id'
            , Ord id'
            ) => (Knowledge a -> Knowledge b) -> sol a -> sol b

  empty :: sol a
  singleton :: Knowledge a -> sol a

  size :: sol a -> Int
  null :: sol a -> Bool
