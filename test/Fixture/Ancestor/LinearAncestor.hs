{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.LinearAncestor
  ( program
  , deltaProgram
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import           Language.Exalog.SemiNaive

import Fixture.Ancestor.Common
import Fixture.Util

{-| Linear ancestor program:
-
- anc(X,Z) :- par(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (ancLit (tvar "X") (tvar "Z")) $ NE.fromList
      [ parLit (tvar "X") (tvar "Y"), ancLit (tvar "Y") (tvar "Z") ]
  , Clause ClABase (ancLit (tvar "X") (tvar "Y")) $ NE.fromList
      [ parLit (tvar "X") (tvar "Y") ]
  ]

{-| Linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- par(X,Y), delta_anc_i(Y,Z).
|-}
deltaProgram :: Program ('ADelta 'ABase)
deltaProgram = Program (decorA ProgABase)
  [ Clause (decorA ClABase) (mkADelta Delta $ ancLit (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkADelta Normal $ parLit (tvar "X") (tvar "Y")
        , mkADelta Delta $ ancLit (tvar "Y") (tvar "Z") ]
  ]
