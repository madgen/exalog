{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Util.Vector
  ( module Util.Nat
  , Vector(..)
  , toList
  ) where

import Util.Nat

infixr 5 :::
data Vector :: Nat -> * -> * where
  Nil   :: Vector 'Zero a
  (:::) :: a -> Vector n a -> Vector ('Succ n) a

toList :: Vector n a -> [ a ]
toList Nil = []
toList (x ::: xs) = x : toList xs
