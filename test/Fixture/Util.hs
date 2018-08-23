module Fixture.Util
  ( tvar
  , tsym
  ) where

import Protolude

import Language.Exalog.Core

-- |Smart constructor for terms
tvar :: Text -> Term
tvar = TVar . Var

tsym :: Text -> Term
tsym = TSym . Sym
