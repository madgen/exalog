{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.NonLinearAncestor
  ( program
  , programSwapped
  , deltaStratum
  , adornedProgram
  , adornedProgramSwapped
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Adornment
import           Language.Exalog.Core
import           Language.Exalog.Delta
import           Language.Exalog.SrcLoc

import Fixture.Ancestor.Common
import Fixture.Util

{-| Non-linear ancestor program:
-
- anc(X,Z) :- anc(X,Y), anc(Y,Z).
- anc(X,Y) :- par(X,Y).
|-}
program :: Program 'ABase
program = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (anc (tvar "X") (tvar "Z")) $ NE.fromList
          [ anc (tvar "X") (tvar "Y"), anc (tvar "Y") (tvar "Z") ]
      , Clause (ClABase dummySpan) (anc (tvar "X") (tvar "Y")) $ NE.fromList
          [ par (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox ancPred
  , PredicateBox parPred
  ]

{-| Non-linear ancestor program deltafied:
-
- delta_{i+1}_anc(X,Z) :- delta_i_anc(X,Y), anc_{i-1}(Y,Z).
- delta_{i+1}_anc(X,Z) :- anc_i(X,Y), delta_i_anc(Y,Z).
|-}
deltaStratum :: Stratum ('ADelta 'ABase)
deltaStratum = Stratum
  [ Clause (decorA (ClABase dummySpan)) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Delta  $ anc (tvar "X") (tvar "Y")
        , mkDeltaLiteral PrevX2 $ anc (tvar "Y") (tvar "Z") ]
  , Clause (decorA (ClABase dummySpan)) (mkDeltaLiteral Delta $ anc (tvar "X") (tvar "Z"))
      $ NE.fromList
        [ mkDeltaLiteral Prev   $ anc (tvar "X") (tvar "Y")
        , mkDeltaLiteral Delta  $ anc (tvar "Y") (tvar "Z") ]
  ]

{-| Non-linear ancestor program adorned:
-
- anc_ff(X,Z) :- anc_ff(X,Y), anc_bf(Y,Z).
- anc_ff(X,Y) :- par_ff(X,Y).
- anc_bf(X,Z) :- anc_bf(X,Y), anc_bf(Y,Z).
- anc_bf(X,Y) :- par_bf(X,Y).
|-}
adornedProgram :: Program ('AAdornment 'ABase)
adornedProgram = Program (decorA (ProgABase dummySpan))
  (Stratum <$>
    [ [ Clause (decorA (ClABase dummySpan)) (adornLiteral [Free, Free] $ anc (tvar "X") (tvar "Z"))
          $ NE.fromList
            [ adornLiteral [Free,  Free] $ anc (tvar "X") (tvar "Y")
            , adornLiteral [Bound, Free] $ anc (tvar "Y") (tvar "Z") ]
      , Clause (decorA (ClABase dummySpan)) (adornLiteral [Bound, Free] $ anc (tvar "X") (tvar "Z"))
          $ NE.fromList
            [ adornLiteral [Bound, Free]   $ anc (tvar "X") (tvar "Y")
            , adornLiteral [Bound, Free]  $ anc (tvar "Y") (tvar "Z") ]
      , Clause (decorA (ClABase dummySpan))
          (adornLiteral [Free, Free] $ anc (tvar "X") (tvar "Y"))
          $ NE.fromList
            [ adornLiteral [Free, Free] $ par (tvar "X") (tvar "Y") ]
      , Clause (decorA (ClABase dummySpan))
          (adornLiteral [Bound, Free] $ anc (tvar "X") (tvar "Y"))
          $ NE.fromList
            [ adornLiteral [Bound, Free] $ par (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox . decorate $ ancPred
  , PredicateBox . decorate $ parPred
  ]

{-| Same non-linear ancestor program except anc atoms are swapped
- (minus base case):
-
- anc(X,Z) :- anc(Y,Z), anc(X,Y).
|-}
programSwapped :: Program 'ABase
programSwapped = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (anc (tvar "X") (tvar "Z")) $ NE.fromList
          [ anc (tvar "Y") (tvar "Z"), anc (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox ancPred
  , PredicateBox parPred
  ]

{-| Adorned swapped non-linear ancestor program:
-
- anc_ff(X,Z) :- anc_ff(Y,Z), anc_fb(X,Y).
- anc_fb(X,Z) :- anc_fb(Y,Z), anc_fb(X,Y).
|-}
adornedProgramSwapped :: Program ('AAdornment 'ABase)
adornedProgramSwapped = Program (decorA $ ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (decorA $ ClABase dummySpan)
          (adornLiteral [Free, Free] $ anc (tvar "X") (tvar "Z"))
          $ NE.fromList
            [ adornLiteral [ Free, Free  ] $ anc (tvar "Y") (tvar "Z")
            , adornLiteral [ Free, Bound ] $ anc (tvar "X") (tvar "Y") ]
      , Clause (decorA $ ClABase dummySpan)
          (adornLiteral [Free, Bound] $ anc (tvar "X") (tvar "Z"))
          $ NE.fromList
            [ adornLiteral [ Free, Bound ] $ anc (tvar "Y") (tvar "Z")
            , adornLiteral [ Free, Bound ] $ anc (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox . decorate $ ancPred
  , PredicateBox . decorate $ parPred
  ]
