{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Exalog.Tuples
  ( Tuples
  , isEmpty
  , fromList, toList
  , difference
  , size
  ) where

import Protolude hiding (toList)

import           Data.List ((\\))
import qualified Data.Set as S
import qualified Data.Vector.Sized as V

import Language.Exalog.Core

newtype Tuples n = Tuples [ V.Vector n Sym ] deriving (Show)

instance Eq (Tuples n) where
  Tuples ts == Tuples ts' = S.fromList ts == S.fromList ts'

instance Semigroup (Tuples n) where
  Tuples ts <> Tuples ts' = Tuples . S.toList . S.fromList $ ts ++ ts'

instance Monoid (Tuples n) where
  mempty = Tuples []

isEmpty :: Tuples n -> Bool
isEmpty (Tuples ts) = null ts

toList :: Tuples n -> [ V.Vector n Sym ]
toList (Tuples ts) = ts

fromList :: [ V.Vector n Sym ] -> Tuples n
fromList = Tuples

difference :: Tuples n -> Tuples n -> Tuples n
Tuples ts `difference` Tuples ts' = Tuples $ ts \\ ts'

size :: Tuples n -> Int
size (Tuples ts) = length ts
