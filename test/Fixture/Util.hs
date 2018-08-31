{-# LANGUAGE DataKinds #-}

module Fixture.Util
  ( tvar
  , tsym
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

tsym :: Text -> Term
tsym = TSym . Sym
