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

newtype Unifier = Unifier [ (Var, Sym) ] deriving (Eq)

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
unify v w = Unifier . catMaybes . V.toList <$> V.zipWithM attempt v w
  where
  attempt :: Term -> Sym -> Maybe (Maybe (Var, Sym))
  attempt (TVar var) sym  = return $ Just (var,sym)
  attempt (TSym sym) sym'
    | sym == sym' = return Nothing
    | otherwise   = Nothing

class Substitutable a where
  substitute :: Unifier -> a -> a

instance Substitutable (V.Vector n Term) where
  substitute (Unifier u) = fmap $ \t ->
    case t of
      TVar v ->
        case v `lookup` u of
          Just s -> TSym s
          Nothing -> t
      TSym{} -> t

instance Substitutable (Literal a) where
  substitute unifier Literal{..} =
    Literal {terms = unifier `substitute` terms, ..}
