{-# LANGUAGE DataKinds #-}

module Fixture.Negation where

import Protolude hiding (not, Set)

import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.KnowledgeBase.Class
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))

import Fixture.Util

rPred, tPred, tcPred :: Predicate 2 'ABase
rPred = Predicate  (PredABase NoSpan) "r" SNat Logical
tPred = Predicate  (PredABase NoSpan) "t" SNat Logical
tcPred = Predicate (PredABase NoSpan) "tc" SNat Logical

vPred :: Predicate 1 'ABase
vPred = Predicate (PredABase NoSpan) "v" SNat Logical

r, t, tc :: Term -> Term -> Literal 'ABase
r  term term' = lit rPred $ fromJust  $ V.fromList [ term, term' ]
t  term term' = lit tPred $ fromJust  $ V.fromList [ term, term' ]
tc term term' = lit tcPred $ fromJust $ V.fromList [ term, term' ]

v :: Term -> Literal 'ABase
v term = lit vPred $ fromJust $ V.fromList [ term ]

{- Compute complement of transitive closure of a graph
-
- v(x)   :- r(x,y)
- v(y)   :- r(x,y).
- t(x,y) :- r(x,y).
- t(x,y) :- t(x,z), r(z,y).
- tc(x,y):- v(x), v(y), not t(x,y).
-}
program :: Program 'ABase
program = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (v (tvar "X")) $ NE.fromList [ r (tvar "X") (tvar "Y") ]
      , Clause (ClABase NoSpan) (v (tvar "Y")) $ NE.fromList [ r (tvar "X") (tvar "Y") ]
      , Clause (ClABase NoSpan) (t (tvar "X") (tvar "Y")) $ NE.fromList
          [ r (tvar "X") (tvar "Y") ]
      , Clause (ClABase NoSpan) (t (tvar "X") (tvar "Y")) $ NE.fromList
          [ t (tvar "X") (tvar "Z"), r (tvar "Z") (tvar "Y") ]
      , Clause (ClABase NoSpan) (tc (tvar "X") (tvar "Y")) $ NE.fromList
          [ v (tvar "X"), v (tvar "Y"), not $ t (tvar "X") (tvar "Y") ]
      ] ])
  [ PredicateBox rPred
  , PredicateBox tPred
  , PredicateBox tcPred
  , PredicateBox vPred
  ]

rKB :: Set 'ABase
rKB = fromList $ (Knowledge KnowABase) rPred . fmap symbol . fromJust . V.fromList <$>
  ([ [ "x"     , "y" ]
  , [ "x"     , "z" ]
  , [ "z"     , "x" ]
  , [ "y"     , "w" ]
  , [ "x"     , "x" ]
  ] :: [ [ Text ] ])

initEDB :: Set 'ABase
initEDB = rKB

vKB :: Set 'ABase
vKB = fromList $ (Knowledge KnowABase) vPred . fmap symbol . fromJust . V.fromList <$>
  ([ [ "x" ], [ "y" ], [ "z" ], [ "w" ] ] :: [ [ Text ] ])

tKB :: Set 'ABase
tKB = fromList $ (Knowledge KnowABase) tPred . fmap symbol . fromJust . V.fromList <$>
  ([ [ "x"     , "x" ]
  , [ "x"     , "y" ]
  , [ "y"     , "w" ]
  , [ "x"     , "w" ]
  , [ "x"     , "z" ]
  , [ "z"     , "x" ]
  , [ "z"     , "y" ]
  , [ "z"     , "w" ]
  , [ "z"     , "z" ]
  ] :: [ [ Text ] ])

tcKB :: Set 'ABase
tcKB = fromList $ (Knowledge KnowABase) tcPred . fmap symbol . fromJust . V.fromList <$>
  ([ [ "y"     , "x" ]
  , [ "y"     , "z" ]
  , [ "w"     , "z" ]
  , [ "w"     , "x" ]
  , [ "y"     , "y" ]
  , [ "w"     , "w" ]
  , [ "w"     , "y" ]
  ] :: [ [ Text ] ])

finalEDB :: Set 'ABase
finalEDB = initEDB <> vKB <> tKB <> tcKB
