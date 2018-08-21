{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Exalog.Relation
  ( Solution
  , Relation
  , Tuples
  -- Smart constructor
  , relation
  -- Helpers
  , findTuples
  , empty
  , add
  , merge
  , rename
  , filter
  , isEmpty
  , isEmpty'
  ) where

import Protolude hiding (empty, filter)

import qualified Data.List as L
import           Data.Singletons.Decide (Decision(..), (%~))

import           Language.Exalog.Core

import           Util.Vector

type Tuples n = [ Vector n Sym ]

data Relation a = forall n. Relation
  { predicate :: Predicate n a
  , tuples    :: Tuples n
  }

type Solution a = [ Relation a ]

empty :: PredicateBox a -> Relation a
empty (PredicateBox p) = Relation p []

isEmpty' :: Solution a -> Bool
isEmpty' = null

isEmpty :: Tuples n -> Bool
isEmpty = null

add :: Eq (PredicateAnn a) => Relation a -> Solution a -> Solution a
add rel [] = [ rel ]
add rel@(Relation p ts) (rel'@(Relation p' ts') : sol)
  | Proved Refl <- sameArity p p'
  , p == p' = Relation p (ts ++ ts') : sol
  | otherwise = rel' : add rel sol

filter :: (forall n. (Predicate n a, Tuples n) -> Bool)
       -> Solution a
       -> Solution a
filter pred = L.filter (\(Relation p ts) -> pred (p,ts))

merge :: Eq (PredicateAnn a) => Solution a -> Solution a -> Solution a
merge sol sol' = foldr add sol' sol

rename :: (forall n. Predicate n a -> Predicate n b) -> Solution a -> Solution b
rename renamer = map (\(Relation p ts) -> Relation (renamer p) ts)

relation :: Predicate n a -> [ Vector n Sym ] -> Relation a
relation = Relation

findTuples :: Eq (PredicateAnn a)
           => Solution a -> Predicate n a -> Maybe (Tuples n)
findTuples [] _ = Nothing
findTuples (Relation p ts : s) p'
  | Proved Refl <- sameArity p p'
  , p == p' = Just ts
  | otherwise = findTuples s p'
