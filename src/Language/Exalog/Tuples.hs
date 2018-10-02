{-# LANGUAGE StandaloneDeriving #-}

module Language.Exalog.Tuples
  ( -- * Types
    Tuples
    -- * Constructors
  , empty
    -- * Conversion
  , fromList, toList
    -- * Predicates
  , isEmpty
    -- * Operations
  , difference
  , size
  ) where

import Protolude hiding (toList, empty)

import qualified Data.Set as S
import qualified Data.Vector.Sized as V

import Language.Exalog.Core

newtype Tuples n = Tuples (S.Set (V.Vector n Sym)) deriving (Show)

deriving instance Ord (Tuples n)

instance Eq (Tuples n) where
  Tuples ts == Tuples ts' = ts == ts'

instance Semigroup (Tuples n) where
  Tuples ts <> Tuples ts' = Tuples $ ts `S.union` ts'

instance Monoid (Tuples n) where
  mempty = Tuples S.empty

empty :: Tuples n
empty = Tuples S.empty

isEmpty :: Tuples n -> Bool
isEmpty (Tuples ts) = S.null ts

toList :: Tuples n -> [ V.Vector n Sym ]
toList (Tuples ts) = S.toList ts

fromList :: [ V.Vector n Sym ] -> Tuples n
fromList = Tuples . S.fromList

difference :: Tuples n -> Tuples n -> Tuples n
Tuples ts `difference` Tuples ts' = Tuples $ ts `S.difference` ts'

size :: Tuples n -> Int
size (Tuples ts) = S.size ts
