{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.KnowledgeBase.Class
  ( Knowledgable(..)
  ) where

import Protolude

import Language.Exalog.Annotation
import Language.Exalog.KnowledgeBase.Knowledge

class (Monad m, Monoid (sol a)) => Knowledgable m sol a where
  fromList :: [ Knowledge a ] -> m (sol a)

  add :: Knowledge a -> sol a -> m (sol a)
  filter :: (Knowledge a -> Bool) -> sol a -> m (sol a)

  atEach :: ( IdentifiableAnn (PredicateAnn b) id'
            , Ord id'
            ) => (Knowledge a -> Knowledge b) -> sol a -> m (sol b)

  empty :: m (sol a)
  singleton :: Knowledge a -> m (sol a)

  size :: sol a -> m Int
  null :: sol a -> m Bool
