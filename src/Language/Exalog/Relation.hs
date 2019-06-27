{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.Relation
  ( -- * Types
    Solution
  , Relation(..)
    -- * Conversion
  , fromList
  , toList
    -- * Search
  , findTuples
  , findTuplesByPredSym
  , filter
  , partition
  , predicates
    -- * Update
  , add
  , merge
  , rename
  , renameM
  , atEach
    -- * Predicates
  , isEmpty
    -- * Misc.
  , size
  ) where

import Protolude hiding (empty, filter, pred, toList)

import qualified Data.List as L
import qualified Data.Set as S
import           Data.Singletons (fromSing)
import           Data.Singletons.Decide (Decision(..))

import qualified GHC.Show as Show

import           Language.Exalog.Core hiding (predicates)
import qualified Language.Exalog.Tuples as T

data Relation a = forall n. Relation (Predicate n a) (T.Tuples n)

deriving instance Show (PredicateAnn a) => Show (Relation a)

instance (IdentifiableAnn (PredicateAnn a) b, Ord b) => Ord (Relation a) where
  Relation p ts `compare` Relation p' ts'
    | Proved Refl <- sameArity p p' =  (p,ts) `compare` (p',ts')
    | otherwise = fromSing (arity p) `compare` fromSing (arity p')

instance Eq (Relation a) where
  Relation p ts == Relation p' ts'
    | Proved Refl <- sameArity p p' = ts == ts'
    | otherwise = False

newtype Solution a = Solution [ Relation a ]

instance ( IdentifiableAnn (PredicateAnn a) b
         , Ord b
         ) => Semigroup (Solution a) where
  Solution sol <> Solution sol' = Solution $ foldr add' sol' sol

instance ( IdentifiableAnn (PredicateAnn a) b
         , Ord b
         ) => Monoid (Solution a) where
  mempty = Solution []

deriving instance ( IdentifiableAnn (PredicateAnn a) b
                  , Ord b
                  ) => Ord (Solution a)

instance Ord (Relation a) => Eq (Solution a) where
  Solution rels == Solution rels' = S.fromList rels == S.fromList rels'

instance ( IdentifiableAnn (PredicateAnn a) b, Ord b
         , Show (PredicateAnn a)
         ) => Show (Solution a) where
  show (Solution rels) = show . S.fromList $ rels

isEmpty :: Solution a -> Bool
isEmpty (Solution xs) = null xs

fromList :: IdentifiableAnn (PredicateAnn a) b => Ord b
         => [ Relation a ] -> Solution a
fromList = mconcat . map (Solution . return)

toList :: Solution a -> [ Relation a ]
toList (Solution rs) = rs

add :: IdentifiableAnn (PredicateAnn a) b => Ord b
    => Relation a -> Solution a -> Solution a
add rel (Solution rs) = Solution $ add' rel rs

add' :: IdentifiableAnn (PredicateAnn a) b
     => Ord b
     => Relation a -> [ Relation a ] -> [ Relation a ]
add' rel [] = [ rel ]
add' rel@(Relation p ts) (rel'@(Relation p' ts') : sol)
  | Proved Refl <- sameArity p p'
  , p == p' = Relation p (ts <> ts') : sol
  | otherwise = rel' : add' rel sol

partition :: (Relation a -> Bool) -> Solution a -> (Solution a, Solution a)
partition p (Solution rs)
  | (xs, ys) <- L.partition p rs = (Solution xs, Solution ys)

predicates :: Solution ann -> [ PredicateBox ann ]
predicates (Solution rs) = map (\(Relation p _) -> PredicateBox p) rs

filter :: (Relation a -> Bool) -> Solution a -> Solution a
filter p (Solution rs) = Solution $ L.filter p rs

merge :: IdentifiableAnn (PredicateAnn a) b => Ord b
      => Solution a -> Solution a -> Solution a
merge (Solution sol) (Solution sol') = Solution $ foldr add' sol' sol

rename :: (forall n. Predicate n a -> Predicate n b) -> Solution a -> Solution b
rename renamer = runIdentity . renameM (pure <$> renamer)

renameM :: Monad m
        => (forall n. Predicate n a -> m (Predicate n b))
        -> Solution a
        -> m (Solution b)
renameM renamer (Solution rs) = Solution <$>
  traverse (\(Relation p ts) -> (`Relation` ts) <$> renamer p) rs

findTuples :: forall a b n
            . IdentifiableAnn (PredicateAnn a) b => Ord b
           => Predicate n a -> Solution a -> T.Tuples n
findTuples p (Solution rs) = go rs
  where
  go :: [ Relation a ] -> T.Tuples n
  go [] = T.fromList []
  go (Relation p' ts : s)
    | Proved Refl <- sameArity p p'
    , p == p' = ts
    | otherwise = go s

findTuplesByPredSym :: PredicateSymbol
                    -> Solution a
                    -> (forall n. T.Tuples n -> IO ())
                    -> IO ()
findTuplesByPredSym predSym (Solution rs) action = go rs
  where
  go :: [ Relation a ] -> IO ()
  go [] = pure ()
  go (Relation p ts : s)
    | predSym == fxSym p = action ts
    | otherwise        = go s

atEach :: (forall n. (Predicate n a, T.Tuples n) -> T.Tuples n)
       -> Solution a
       -> Solution a
atEach f (Solution rs) =
  Solution $ map (\(Relation p ts) -> Relation p $ f (p, ts)) rs

size :: Solution a -> Int
size (Solution rs) = foldr (\(Relation _ ts) -> (T.size ts +)) 0 rs
