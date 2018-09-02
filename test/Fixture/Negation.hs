{-# LANGUAGE DataKinds #-}

module Fixture.Negation
  ( program
  , initEDB
  , tcTuples
  , tcPred
  ) where

import Protolude hiding (not)

import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.Relation
import qualified Language.Exalog.Tuples as T

import Fixture.Util

rPred, tPred, tcPred :: Predicate 2 'ABase
rPred = Predicate PredABase "r" SNat Logical
tPred = Predicate PredABase "t" SNat Logical
tcPred = Predicate PredABase "tc" SNat Logical

vPred :: Predicate 1 'ABase
vPred = Predicate PredABase "v" SNat Logical

r, t, tc :: Term -> Term -> Literal 'ABase
r t t' = lit rPred $ fromJust $ V.fromList [ t, t' ]
t t t' = lit tPred $ fromJust $ V.fromList [ t, t' ]
tc t t' = lit tcPred $ fromJust $ V.fromList [ t, t' ]

v :: Term -> Literal 'ABase
v t = lit vPred $ fromJust $ V.fromList [ t ]

{-
- v(x)   :- r(x,y)
- v(y)   :- r(x,y).
- t(x,y) :- r(x,y).
- t(x,y) :- t(x,z), r(z,y).
- tc(x,y):- v(x), v(y), not t(x,y).
-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (v (tvar "X")) $ NE.fromList [ r (tvar "X") (tvar "Y") ]
  , Clause ClABase (v (tvar "Y")) $ NE.fromList [ r (tvar "X") (tvar "Y") ]
  , Clause ClABase (t (tvar "X") (tvar "Y")) $ NE.fromList
      [ r (tvar "X") (tvar "Y") ]
  , Clause ClABase (t (tvar "X") (tvar "Y")) $ NE.fromList
      [ t (tvar "X") (tvar "Z"), r (tvar "Z") (tvar "Y") ]
  , Clause ClABase (tc (tvar "X") (tvar "Y")) $ NE.fromList
      [ v (tvar "X"), v (tvar "Y"), not $ t (tvar "X") (tvar "Y") ]
  ]

rTuples :: [ V.Vector 2 Text ]
rTuples = fromJust . V.fromList <$>
  [ [ "x"     , "y" ]
  , [ "x"     , "z" ]
  , [ "z"     , "x" ]
  , [ "y"     , "w" ]
  , [ "x"     , "x" ]
  ]

rRel :: Relation 'ABase
rRel = Relation rPred . T.fromList $ fmap Sym <$> rTuples

initEDB :: Solution 'ABase
initEDB = [ rRel ]

tcTuples :: T.Tuples 2
tcTuples = T.fromList $ fmap Sym . fromJust . V.fromList <$>
  [ [ "y"     , "x" ]
  , [ "y"     , "z" ]
  , [ "z"     , "y" ]
  , [ "w"     , "z" ]
  , [ "x"     , "w" ]
  , [ "w"     , "x" ]
  , [ "y"     , "y" ]
  , [ "w"     , "w" ]
  , [ "z"     , "z" ]
  ]
