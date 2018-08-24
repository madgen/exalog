{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.NonLinearAncestor
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

{-| Linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- delta_i_anc(X,Y), anc_{i-1}(Y,Z).
- delta_{i+1}_anc(X,Z) :- anc_i(X,Y), delta_i_anc(Y,Z).
|-}
deltaProgram :: Program ('ADelta 'ABase)
deltaProgram = Program (decorA ProgABase)
  [ Clause (decorA ClABase) (mkADelta Delta $ ancLit (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkADelta Delta  $ ancLit (tvar "X") (tvar "Y")
        , mkADelta PrevX2 $ ancLit (tvar "Y") (tvar "Z") ]
  , Clause (decorA ClABase) (mkADelta Delta $ ancLit (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkADelta Prev   $ ancLit (tvar "X") (tvar "Y")
        , mkADelta Delta  $ ancLit (tvar "Y") (tvar "Z") ]
  ]
