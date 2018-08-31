{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.LinearAncestor
  ( program
  , deltaProgram
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import           Language.Exalog.Delta

import Fixture.Ancestor.Common
import Fixture.Util

{-| Linear ancestor program:
-
- anc(X,Z) :- par(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (anc (tvar "X") (tvar "Z")) $ NE.fromList
      [ par (tvar "X") (tvar "Y"), anc (tvar "Y") (tvar "Z") ]
  , Clause ClABase (anc (tvar "X") (tvar "Y")) $ NE.fromList
      [ par (tvar "X") (tvar "Y") ]
  ]

{-| Linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- par(X,Y), delta_anc_i(Y,Z).
|-}
deltaProgram :: Program ('ADelta 'ABase)
deltaProgram = Program (decorA ProgABase)
  [ Clause (decorA ClABase) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Normal $ par (tvar "X") (tvar "Y")
        , mkDeltaLiteral Delta $ anc (tvar "Y") (tvar "Z") ]
  ]
