{-# LANGUAGE DataKinds #-}

module Fixture.Unification
  ( program
  , initEDB
  , samePred
  , sameTuples
  ) where

import Protolude hiding (Set)

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.KnowledgeBase.Class
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.SrcLoc

import Fixture.Util

samePred :: Predicate 2 'ABase
samePred = Predicate (PredABase NoSpan) "same" SNat Logical

fxPred :: Predicate 3 'ABase
fxPred = Predicate (PredABase NoSpan) "fx" SNat Logical

same :: Term -> Term -> Literal 'ABase
same t t' = lit samePred $ fromJust $ V.fromList [ t, t' ]

fx :: Term -> Term -> Term -> Literal 'ABase
fx t t' t'' = lit fxPred $ fromJust $ V.fromList [ t, t', t'' ]

{-
 - fx(A, B, C) :- fx(A', B, C), same(A, A').
 - fx(A, B, C) :- fx(A, B', C), same(B, B').
 - fx(A, B, C) :- fx(A, B, C'), same(C, C').

  same(A, A') :- fx(A, B, C), fx(A', B, C).
  same(C, C') :- fx(A, _, C), fx(A, _, C').
  same(B, B') :- fx(A, B, _), fx(A, B', _).
-}
program :: Program 'ABase
program = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (fx (tvar "A") (tvar "B") (tvar "C")) $ NE.fromList [ fx (tvar "A'") (tvar "B")  (tvar "C"),  same (tvar "A") (tvar "A'") ]
      , Clause (ClABase NoSpan) (fx (tvar "A") (tvar "B") (tvar "C")) $ NE.fromList [ fx (tvar "A")  (tvar "B'") (tvar "C"),  same (tvar "B") (tvar "B'") ]
      , Clause (ClABase NoSpan) (fx (tvar "A") (tvar "B") (tvar "C")) $ NE.fromList [ fx (tvar "A")  (tvar "B")  (tvar "C'"), same (tvar "C") (tvar "C'") ]

      , Clause (ClABase NoSpan) (same (tvar "A") (tvar "A'")) $ NE.fromList [ fx (tvar "A") (tvar "B") (tvar "C"), fx (tvar "A'") (tvar "B") (tvar "C") ]
      , Clause (ClABase NoSpan) (same (tvar "C") (tvar "C'")) $ NE.fromList [ fx (tvar "A") TWild (tvar "C"), fx (tvar "A") TWild (tvar "C'") ]
      , Clause (ClABase NoSpan) (same (tvar "B") (tvar "B'")) $ NE.fromList [ fx (tvar "A") (tvar "B") TWild, fx (tvar "A'") (tvar "B'") TWild ]
      ]
    ])
  [ PredicateBox samePred, PredicateBox fxPred ]

sameTuples :: [ V.Vector 2 Text ]
sameTuples = fromJust . V.fromList <$>
  [ [ "1"     , "5" ]
  , [ "1"     , "7" ]
  , [ "12"    , "14" ]
  , [ "3"     , "9" ] -- Remove for mildly bad performance
  ]

fxTuples :: [ V.Vector 3 Text ]
fxTuples = fromJust . V.fromList <$>
  [ [ "12"  , "1"   , "8" ]  -- Remove for mildly bad performance
  , [ "13"  , "10"  , "10" ] -- Remove for mildly bad performance
  , [ "14"  , "13"  , "11" ] -- Remove for mildly bad performance
  , [ "4"   , "2"   , "2" ]
  , [ "5"   , "4"   , "3" ]
  , [ "7"   , "int" , "6" ]
  , [ "9"   , "6"   , "8" ]
  ]

initEDB :: Set 'ABase
initEDB = fromList
        $ (Knowledge KnowABase samePred . fmap symbol <$> sameTuples)
       <> (Knowledge KnowABase fxPred . fmap symbol <$> fxTuples)
