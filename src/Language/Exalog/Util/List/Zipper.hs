{-# LANGUAGE DeriveFunctor #-}

module Language.Exalog.Util.List.Zipper
  ( Zipper
  , focus, left, right
  , leftMaybe, rightMaybe
  , fromNonEmptyList, toNonEmptyList
  , fromListMaybe, toList
  , threeWayMap
  ) where

import Protolude hiding (toList)

import qualified Data.List.NonEmpty as NE

import           Control.Comonad (Comonad(..))

data Zipper a = Zipper [ a ] a [ a ] deriving (Functor)

fromNonEmptyList :: NE.NonEmpty a -> Zipper a
fromNonEmptyList (a NE.:| as) = Zipper [] a as

toNonEmptyList :: Zipper a -> NE.NonEmpty a
toNonEmptyList (Zipper ls a rs) =
  case reverse ls of
    []     -> a NE.:| rs
    (x:xs) -> x NE.:| xs ++ a : rs

toList :: Zipper a -> [ a ]
toList (Zipper ls a rs) = reverse ls ++ a : rs

fromListMaybe :: [ a ] -> Maybe (Zipper a)
fromListMaybe [] = Nothing
fromListMaybe (x:xs) = Just $ Zipper [] x xs

focus :: Zipper a -> a
focus (Zipper _ a _) = a

leftMaybe :: Zipper a -> Maybe (Zipper a)
leftMaybe (Zipper (l:ls) a rs) = Just $ Zipper ls l (a:rs)
leftMaybe _ = Nothing

rightMaybe :: Zipper a -> Maybe (Zipper a)
rightMaybe (Zipper ls a (r:rs)) = Just $ Zipper (a:ls) r rs
rightMaybe _ = Nothing

left :: Zipper a -> Zipper a
left z = fromMaybe z (leftMaybe z)

right :: Zipper a -> Zipper a
right z = fromMaybe z (rightMaybe z)

threeWayMap :: (a -> b) -> (a -> b) -> (a -> b) -> Zipper a -> Zipper b
threeWayMap f g h (Zipper ls a rs) = Zipper (fmap f ls) (g a) (fmap h rs)

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate w = Zipper (go' leftMaybe w) w (go' rightMaybe w)
    where
    go' :: (Zipper a -> Maybe (Zipper a)) -> Zipper a -> [ Zipper a ]
    go' f = unfoldr (fmap (\s -> (s,s)) . f)
