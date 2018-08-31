{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.NonLinearAncestor
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
- anc(X,Z) :- anc(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (anc (tvar "X") (tvar "Z")) $ NE.fromList
      [ anc (tvar "X") (tvar "Y"), anc (tvar "Y") (tvar "Z") ]
  , Clause ClABase (anc (tvar "X") (tvar "Y")) $ NE.fromList
      [ par (tvar "X") (tvar "Y") ]
  ]

{-| Linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- delta_i_anc(X,Y), anc_{i-1}(Y,Z).
- delta_{i+1}_anc(X,Z) :- anc_i(X,Y), delta_i_anc(Y,Z).
|-}
deltaProgram :: Program ('ADelta 'ABase)
deltaProgram = Program (decorA ProgABase)
  [ Clause (decorA ClABase) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Delta  $ anc (tvar "X") (tvar "Y")
        , mkDeltaLiteral PrevX2 $ anc (tvar "Y") (tvar "Z") ]
  , Clause (decorA ClABase) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Prev   $ anc (tvar "X") (tvar "Y")
        , mkDeltaLiteral Delta  $ anc (tvar "Y") (tvar "Z") ]
  ]
