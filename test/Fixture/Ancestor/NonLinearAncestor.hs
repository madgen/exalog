{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.NonLinearAncestor
  ( program
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core

import Fixture.Ancestor.Common
import Fixture.Util

{-| Linear ancestor program:
-
- anc(X,Z) :- anc(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (ancLit (tvar "X") (tvar "Z")) $ NE.fromList
      [ ancLit (tvar "X") (tvar "Y"), ancLit (tvar "Y") (tvar "Z") ]
  , Clause ClABase (ancLit (tvar "X") (tvar "Y")) $ NE.fromList
      [ parLit (tvar "X") (tvar "Y") ]
  ]
