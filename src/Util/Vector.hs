{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Util.Vector
  ( module Util.Nat
  , Vector(..)
  , toList
  ) where

import Protolude hiding (Nat, toList)

import Util.Nat

infixr 5 :::
data Vector :: Nat -> * -> * where
  Nil   :: Vector 'Zero a
  (:::) :: a -> Vector n a -> Vector ('Succ n) a

deriving instance Eq a => Eq (Vector n a)
deriving instance Ord a => Ord (Vector n a)
deriving instance Show a => Show (Vector n a)
deriving instance Functor (Vector n)

toList :: Vector n a -> [ a ]
toList Nil = []
toList (x ::: xs) = x : toList xs
