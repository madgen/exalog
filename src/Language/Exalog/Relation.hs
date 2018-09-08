{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , atEach
  , size
  ) where

import Protolude hiding (empty, filter, pred)

import qualified Data.List as L
import qualified Data.Set as S
import           Data.Singletons (fromSing)
import           Data.Singletons.Decide (Decision(..))

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T

data Relation a = forall n. Relation (Predicate n a) (T.Tuples n)

deriving instance Show (PredicateAnn a) => Show (Relation a)

instance Ord (PredicateAnn a) => Ord (Relation a) where
  Relation p ts `compare` Relation p' ts'
    | Proved Refl <- sameArity p p' =  (p,ts) `compare` (p',ts')
    | otherwise = fromSing (arity p) `compare` fromSing (arity p')

instance Eq (Relation a) where
  Relation p ts == Relation p' ts'
    | Proved Refl <- sameArity p p' = ts == ts'
    | otherwise = False

newtype Solution a = Solution [ Relation a ]

deriving instance Ord (PredicateAnn a) => Ord (Solution a)

instance Ord (Relation a) => Eq (Solution a) where
  Solution rels == Solution rels' = S.fromList rels == S.fromList rels'

deriving instance Show (PredicateAnn a) => Show (Solution a)

isEmpty :: Solution a -> Bool
isEmpty (Solution xs) = null xs

fromList :: [ Relation a ] -> Solution a
fromList = Solution

add :: Eq (PredicateAnn a) => Relation a -> Solution a -> Solution a
add rel (Solution rs) = Solution $ add' rel rs

add' :: Eq (PredicateAnn a) => Relation a -> [ Relation a ] -> [ Relation a ]
add' rel [] = [ rel ]
add' rel@(Relation p ts) (rel'@(Relation p' ts') : sol)
  | Proved Refl <- sameArity p p'
  , p == p' = Relation p (ts <> ts') : sol
  | otherwise = rel' : add' rel sol

filter :: (Relation a -> Bool) -> Solution a -> Solution a
filter p (Solution rs) = Solution $ L.filter p rs

merge :: Eq (PredicateAnn a) => Solution a -> Solution a -> Solution a
merge (Solution sol) (Solution sol') = Solution $ foldr add' sol' sol

rename :: (forall n. Predicate n a -> Predicate n b) -> Solution a -> Solution b
rename renamer (Solution rs) =
  Solution $ map (\(Relation p ts) -> Relation (renamer p) ts) rs

findTuples :: forall a n. Eq (PredicateAnn a)
           => Solution a -> Predicate n a -> T.Tuples n
findTuples (Solution rs) = findTuples' rs
  where
  findTuples' :: [ Relation a ] -> Predicate n a -> T.Tuples n
  findTuples' [] _ = T.fromList []
  findTuples' (Relation p ts : s) p'
    | Proved Refl <- sameArity p p'
    , p == p' = ts
    | otherwise = findTuples' s p'

atEach :: (forall n. (Predicate n a, T.Tuples n) -> T.Tuples n)
       -> Solution a
       -> Solution a
atEach f (Solution rs) =
  Solution $ map (\(Relation p ts) -> Relation p $ f (p, ts)) rs

size :: Solution a -> Int
size (Solution rs) = foldr (\(Relation _ ts) -> (T.size ts +)) 0 rs
