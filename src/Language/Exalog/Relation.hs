{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Exalog.Relation
  ( Solution
  , Relation
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
  ) where

import Protolude hiding (empty, filter)

import qualified Data.List as L
import           Data.Singletons.Decide (Decision(..))

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T

import           Util.Vector

data Relation a = forall n. Relation (Predicate n a) (T.Tuples n)

type Solution a = [ Relation a ]

empty :: PredicateBox a -> Relation a
empty (PredicateBox p) = Relation p []

isEmpty :: Solution a -> Bool
isEmpty = null

add :: Eq (PredicateAnn a) => Relation a -> Solution a -> Solution a
add rel [] = [ rel ]
add rel@(Relation p ts) (rel'@(Relation p' ts') : sol)
  | Proved Refl <- sameArity p p'
  , p == p' = Relation p (ts ++ ts') : sol
  | otherwise = rel' : add rel sol

filter :: (forall n. (Predicate n a, T.Tuples n) -> Bool)
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
           => Solution a -> Predicate n a -> Maybe (T.Tuples n)
findTuples [] _ = Nothing
findTuples (Relation p ts : s) p'
  | Proved Refl <- sameArity p p'
  , p == p' = Just ts
  | otherwise = findTuples s p'
