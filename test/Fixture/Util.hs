{-# LANGUAGE DataKinds #-}

module Fixture.Util
  ( tvar
  , tsym
  , symbol
  , lit
  , not
  ) where

import Protolude hiding (pred, not)

import qualified Data.Vector.Sized as V

import Language.Exalog.Core

not :: Literal 'ABase -> Literal 'ABase
not l@Literal{polarity = pol} =
  l { polarity = if pol == Positive then Negative else Positive }

lit :: Predicate n 'ABase -> V.Vector n Term -> Literal 'ABase
lit = Literal LitABase Positive

-- |Smart constructor for terms
tvar :: Text -> Term
tvar = TVar . Var

class Termable a where
  symbol :: a -> Sym
  tsym   :: a -> Term
  tsym = TSym . symbol
  {-# MINIMAL symbol #-}

instance Termable Text where
  symbol = SymText

instance Termable Int where
  symbol = SymInt

instance Termable Float where
  symbol = SymFloat

instance Termable Bool where
  symbol = SymBool
