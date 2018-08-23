{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.Relation
  ( Solution
  , Relation(..)
  -- Helpers
  , fromList
  , findTuples
  , add
  , merge
  , rename
  , filter
  , isEmpty
  , size
  ) where

import Protolude hiding (empty, filter)

import qualified Data.List as L
import           Data.Singletons.Decide (Decision(..))

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T

import           Util.Vector

data Relation a = forall n. Relation (Predicate n a) (T.Tuples n)

deriving instance Show (PredicateAnn a) => Show (Relation a)

instance Eq (Relation a) where
  Relation p ts == Relation p' ts'
    | Proved Refl <- sameArity p p' = ts == ts'
    | otherwise = False

type Solution a = [ Relation a ]

isEmpty :: Solution a -> Bool
isEmpty = null

fromList :: [ Relation a ] -> Solution a
fromList rs = rs

add :: Eq (PredicateAnn a) => Relation a -> Solution a -> Solution a
add rel [] = [ rel ]
add rel@(Relation p ts) (rel'@(Relation p' ts') : sol)
  | Proved Refl <- sameArity p p'
  , p == p' = Relation p (ts <> ts') : sol
  | otherwise = rel' : add rel sol

filter :: (forall n. (Predicate n a, T.Tuples n) -> Bool)
       -> Solution a
       -> Solution a
filter pred = L.filter (\(Relation p ts) -> pred (p,ts))

merge :: Eq (PredicateAnn a) => Solution a -> Solution a -> Solution a
merge sol sol' = foldr add sol' sol

rename :: (forall n. Predicate n a -> Predicate n b) -> Solution a -> Solution b
rename renamer = map (\(Relation p ts) -> Relation (renamer p) ts)

findTuples :: Eq (PredicateAnn a) => Solution a -> Predicate n a -> T.Tuples n
findTuples [] _ = T.fromList []
findTuples (Relation p ts : s) p'
  | Proved Refl <- sameArity p p'
  , p == p' = ts
  | otherwise = findTuples s p'

size :: Solution a -> Int
size = foldr (\(Relation _ ts) -> (T.size ts +)) 0
