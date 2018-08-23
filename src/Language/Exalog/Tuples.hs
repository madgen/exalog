{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Exalog.Tuples
  ( Tuples
  , isEmpty
  , fromList, toList
  ) where

import Protolude hiding (toList)

import qualified Data.Set as S

import Language.Exalog.Core
import Util.Vector hiding (toList)

newtype Tuples n = Tuples [ Vector n Sym ]

instance Eq (Tuples n) where
  Tuples ts == Tuples ts' = S.fromList ts == S.fromList ts'

instance Semigroup (Tuples n) where
  Tuples ts <> Tuples ts' = Tuples $ ts ++ ts'

instance Monoid (Tuples n) where
  mempty = Tuples []

isEmpty :: Tuples n -> Bool
isEmpty (Tuples ts) = null ts

toList :: Tuples n -> [ Vector n Sym ]
toList (Tuples ts) = ts

fromList :: [ Vector n Sym ] -> Tuples n
fromList = Tuples
