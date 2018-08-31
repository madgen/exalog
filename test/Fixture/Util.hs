{-# LANGUAGE DataKinds #-}

module Fixture.Util
  ( tvar
  , tsym
  , lit
  ) where

import Protolude hiding (pred)

import qualified Data.Vector.Sized as V

import Language.Exalog.Core

lit :: Predicate n 'ABase -> V.Vector n Term -> Literal 'ABase
lit = Literal LitABase Positive

-- |Smart constructor for terms
tvar :: Text -> Term
tvar = TVar . Var

tsym :: Text -> Term
tsym = TSym . Sym
