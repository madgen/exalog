{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Exalog.KnowledgeBase.Knowledge where

import qualified Data.Vector.Sized as V

import Language.Exalog.Core

data Knowledge a = forall n. Knowledge (Predicate n a) (V.Vector n Sym)
