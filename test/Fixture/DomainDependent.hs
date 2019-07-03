{-# LANGUAGE DataKinds #-}

module Fixture.DomainDependent
  ( programGood
  , programBad1
  , programBad2
  ) where

import Protolude hiding (SrcLoc)

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.SrcLoc

import Fixture.Util

cPred, rPred :: Predicate 2 'ABase
cPred = Predicate (PredABase dummySpan) "c" SNat Logical
rPred = Predicate (PredABase dummySpan) "r" SNat Logical

c, r :: Term -> Term -> Literal 'ABase
c t t' = lit cPred $ fromJust $ V.fromList [ t, t' ]
r t t' = lit rPred $ fromJust $ V.fromList [ t, t' ]

{-
- r(X,Y) :- c(X,Y).
-}
programGood :: Program 'ABase
programGood = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (r  (tvar "X") (tvar "Y")) $
          NE.fromList [ c (tvar "X") (tvar "Y") ]
      ]
    ])
  []

{-
- r(X,Y) :- c(X,X).
-}
programBad1 :: Program 'ABase
programBad1 = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (r  (tvar "X") (tvar "Y")) $
          NE.fromList [ c (tvar "X") (tvar "X") ]
      ]
    ])
  []

{-
- r(X,Y) :- c("a","b").
-}
programBad2 :: Program 'ABase
programBad2 = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (r  (tvar "X") (tvar "Y")) $
          NE.fromList [ c (tsym ("a" :: Text)) (tsym ("b" :: Text)) ]
      ]
    ])
  []
