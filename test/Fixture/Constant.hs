{-# LANGUAGE DataKinds #-}

module Fixture.Constant
  ( program
  , initEDB
  , rPred
  , rTuples
  ) where

import Protolude

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.Relation
import qualified Language.Exalog.Tuples as T

import Fixture.Util

cPred, rPred :: Predicate 2 'ABase
cPred = Predicate PredABase "c" SNat Logical
rPred = Predicate PredABase "r" SNat Logical

c,r :: Term -> Term -> Literal 'ABase
c t t' = lit cPred $ fromJust $ V.fromList [ t, t' ]
r t t' = lit rPred $ fromJust $ V.fromList [ t, t' ]

{-
- r("c","1") :- c("a","b").
- r(X  ,"2") :- c("a",X).
- r("c","3") :- c("q","b").
- r("e","4") :- r(X,Y), c("a",X).
- r("f","5") :- c("a",X), r(X,Y).
-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (r (tsym "c")  (tsym "1")) $ NE.fromList [ c (tvar "a") (tvar "b") ]
  , Clause ClABase (r (tvar "X")  (tsym "2")) $ NE.fromList [ c (tsym "a") (tvar "X") ]
  , Clause ClABase (r (tsym "c")  (tsym "3")) $ NE.fromList [ c (tsym "q") (tsym "b") ]
  , Clause ClABase (r (tsym "e")  (tsym "4")) $ NE.fromList
    [ r (tvar "X") (tvar "Y")
    , c (tsym "a") (tvar "X") ]
  , Clause ClABase (r (tsym "f")  (tsym "5")) $ NE.fromList
    [ c (tsym "a") (tvar "X")
    , r (tvar "X") (tvar "Y") ]
  ]

cTuples :: [ V.Vector 2 Text ]
cTuples = fromJust . V.fromList <$>
  [ [ "a"     , "b" ]
  , [ "a"     , "c" ]
  , [ "a"     , "d" ]
  ]

cRel :: Relation 'ABase
cRel = Relation cPred . T.fromList $ fmap Sym <$> cTuples

initEDB :: Solution 'ABase
initEDB = fromList [ cRel ]

rTuples :: T.Tuples 2
rTuples = T.fromList $ fmap Sym . fromJust . V.fromList <$>
  [ [ "c", "1" ]
  , [ "b", "2" ]
  , [ "c", "2" ]
  , [ "d", "2" ]
  , [ "e", "4" ]
  , [ "f", "5" ]
  ]
