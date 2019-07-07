{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Exalog.Unification
  ( Unifier
  , empty
  , extend
  , substitute
  , unify
  ) where

import Protolude hiding (empty, sym)

import           Data.List (lookup)
import qualified Data.Vector.Sized as V

import Language.Exalog.Core

newtype Unifier = Unifier [ (Var, Sym) ] deriving (Eq, Show)

empty :: Unifier
empty = Unifier []

extend :: Unifier -> Unifier -> Maybe Unifier
Unifier us `extend` Unifier us' = Unifier <$> us `extend'` us'

extend' :: [ (Var, Sym) ] -> [ (Var, Sym) ] -> Maybe [ (Var, Sym) ]
extend' [] u' = Just u'
extend' (binding@(v,s) : u) u' =
  case v `lookup` u' of
    Just s' -> if s == s' then extend' u u' else Nothing
    Nothing -> (binding:) <$> extend' u u'

unify :: V.Vector n Term -> V.Vector n Sym -> Maybe Unifier
unify v w = Unifier <$> foldr' attempt (Just []) (V.zip v w)
  where
  attempt :: (Term, Sym) -> Maybe [ (Var, Sym) ] -> Maybe [ (Var, Sym) ]
  attempt _                Nothing               = Nothing
  attempt (TWild, _)       mus                   = mus
  attempt (TSym sym, sym') mus     | sym == sym' = mus
                                   | otherwise   = Nothing
  attempt (TVar var, sym)  mus@(Just unifierAcc) =
    case var `lookup` unifierAcc of
      Just sym'
        | sym == sym' -> mus
        | otherwise   -> Nothing
      Nothing         -> ((var,sym) :) <$> mus

class Substitutable a where
  substitute :: Unifier -> a -> a

instance Substitutable (V.Vector n Term) where
  substitute (Unifier u) = fmap $ \t ->
    case t of
      TVar v -> maybe t TSym (v `lookup` u)
      TSym{} -> t
      TWild  -> t

instance Substitutable (Literal a) where
  substitute unifier Literal{..} =
    Literal {_terms = unifier `substitute` _terms, ..}
