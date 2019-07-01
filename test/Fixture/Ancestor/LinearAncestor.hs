{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.LinearAncestor
  ( program
  , deltaStratum
  , adornedProgram
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import           Language.Exalog.Delta
import           Language.Exalog.Adornment
import           Language.Exalog.SrcLoc

import Fixture.Ancestor.Common
import Fixture.Util

{-| Linear ancestor program:
-
- anc(X,Z) :- par(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program (ProgABase dummySpan)
  [ [ Clause (ClABase dummySpan) (anc (tvar "X") (tvar "Z")) $ NE.fromList
        [ par (tvar "X") (tvar "Y"), anc (tvar "Y") (tvar "Z") ]
    , Clause (ClABase dummySpan) (anc (tvar "X") (tvar "Y")) $ NE.fromList
        [ par (tvar "X") (tvar "Y") ]
    ]
  ] [ PredicateBox ancPred, PredicateBox parPred ]

{-| Linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- par(X,Y), delta_anc_i(Y,Z).
|-}
deltaStratum :: [ Clause ('ADelta 'ABase) ]
deltaStratum =
  [ Clause (decorA (ClABase dummySpan)) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Normal $ par (tvar "X") (tvar "Y")
        , mkDeltaLiteral Delta $ anc (tvar "Y") (tvar "Z") ]
  ]

{-| Linear ancestor program adorned:
-
- anc_ff(X,Z) :- par_ff(X,Y), anc_bf(Y,Z).
- anc_bf(X,Z) :- par_bf(X,Y), anc_bf(Y,Z).
- anc_ff(X,Y) :- par_ff(X,Y).
- anc_bf(X,Y) :- par_bf(X,Y).
|-}
adornedProgram :: Program ('AAdornment 'ABase)
adornedProgram = Program (decorA (ProgABase dummySpan))
  [ [ Clause (decorA (ClABase dummySpan))
        (adornLiteral [ Free, Free ] $ anc (tvar "X") (tvar "Z"))
        $ NE.fromList
          [ adornLiteral [ Free, Free ] $ par (tvar "X") (tvar "Y")
          , adornLiteral [ Bound, Free ] $ anc (tvar "Y") (tvar "Z") ]
    , Clause (decorA (ClABase dummySpan))
        (adornLiteral [ Bound, Free ] $ anc (tvar "X") (tvar "Z"))
        $ NE.fromList
          [ adornLiteral [ Bound, Free ] $ par (tvar "X") (tvar "Y")
          , adornLiteral [ Bound, Free ] $ anc (tvar "Y") (tvar "Z") ]
    , Clause (decorA (ClABase dummySpan))
        (adornLiteral [ Free, Free ] $ anc (tvar "X") (tvar "Y"))
        $ NE.fromList
          [ adornLiteral [ Free, Free ] $ par (tvar "X") (tvar "Y") ]
    , Clause (decorA (ClABase dummySpan))
        (adornLiteral [ Bound, Free ] $ anc (tvar "X") (tvar "Y"))
        $ NE.fromList
          [ adornLiteral [ Bound, Free ] $ par (tvar "X") (tvar "Y") ]
    ]
  ]
  [ PredicateBox . decorate $ ancPred
  , PredicateBox . decorate $ parPred
  ]
