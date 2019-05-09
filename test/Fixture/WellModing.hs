{-# LANGUAGE DataKinds #-}

module Fixture.WellModing where

import Protolude hiding (pred, guard, not)

import           Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.SrcLoc
import           Language.Exalog.Renamer

import Fixture.Util hiding (lit)

pred :: Int -> PredicateSymbol -> SNat n -> Nature n -> Predicate n ('ARename 'ABase)
pred id = Predicate (PredARename (PredicateID id) $ PredABase dummySpan)
lit :: Int -> Polarity -> Predicate n ('ARename 'ABase) -> V.Vector n Term -> Literal ('ARename 'ABase)
lit  id = Literal   (LitARename  (LiteralID   id) $ LitABase  dummySpan)
cl :: Int -> Head ('ARename 'ABase) -> Body ('ARename 'ABase) -> Clause ('ARename 'ABase)
cl   id = Clause    (ClARename   (ClauseID    id) $ ClABase   dummySpan)

pPred, qPred, aPred, guardPred :: Predicate 1 ('ARename 'ABase)
pPred     = pred 0 "p" SNat Logical
qPred     = pred 1 "q" SNat Logical
aPred     = pred 2 "q" SNat Logical
guardPred = pred 3 "guard0" SNat Logical

queryPred :: Predicate 0 ('ARename 'ABase)
queryPred = pred 4 "query" SNat Logical

p, notq, a, guard :: Int -> Term -> Literal ('ARename 'ABase)
p     id t = lit id Positive pPred     (fromJust $ V.fromList [ t ])
notq  id t = lit id Negative qPred     (fromJust $ V.fromList [ t ])
a     id t = lit id Positive aPred     (fromJust $ V.fromList [ t ])
guard id t = lit id Positive guardPred (fromJust $ V.fromList [ t ])

query :: Int -> Literal ('ARename 'ABase)
query id = lit id Positive queryPred (fromJust $ V.fromList [ ])

{-|
- p(X) :- ! q(X).
- query() :- a(X), p(X).
|-}
prSimple :: Program ('ARename 'ABase)
prSimple = Program (ProgARename $ ProgABase dummySpan)
  [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ notq 99 (tvar "X") ]
  , cl 200 (query 20)        $ NE.fromList [ a 30 (tvar "X"), p 40 (tvar "X") ]
  ] [ PredicateBox queryPred ]

{-|
- p(X) :- ! q(X).
- query() :- a(X), p(X).
|-}
prSimpleRepaired :: Program 'ABase
prSimpleRepaired = peel $ Program (ProgARename $ ProgABase dummySpan)
  [ cl 100 (p 10 (tvar "X"))  $ NE.fromList [ guard 50 (tvar "X"), notq 99 (tvar "X") ]
  , cl 200 (query 20)         $ NE.fromList [ a 30 (tvar "X")    , p 40 (tvar "X") ]
  , cl 300 (guard 70 (tvar "X")) $ NE.fromList [ a 60 (tvar "X") ]
  ] [ PredicateBox queryPred ]
