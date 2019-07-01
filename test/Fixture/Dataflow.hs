{-# LANGUAGE DataKinds #-}

module Fixture.Dataflow where

import Protolude hiding (pred)

import           Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.Dataflow
import           Language.Exalog.SrcLoc
import           Language.Exalog.Renamer

import Fixture.Util hiding (lit)

pred :: Int -> PredicateSymbol -> SNat n -> Nature n -> Predicate n ('ARename 'ABase)
pred id = Predicate (PredARename (PredicateID id) $ PredABase dummySpan)
lit :: Int -> Polarity -> Predicate n ('ARename 'ABase) -> V.Vector n Term -> Literal ('ARename 'ABase)
lit  id = Literal   (LitARename  (LiteralID   id) $ LitABase  dummySpan)
cl :: Int -> Head ('ARename 'ABase) -> Body ('ARename 'ABase) -> Clause ('ARename 'ABase)
cl   id = Clause    (ClARename   (ClauseID    id) $ ClABase   dummySpan)

pPred, qPred, sPred, aPred :: Predicate 1 ('ARename 'ABase)
pPred = pred 0 "p" SNat Logical
qPred = pred 1 "q" SNat Logical
sPred = pred 2 "s" SNat Logical
aPred = pred 3 "a" SNat Logical

rPred :: Predicate 2 ('ARename 'ABase)
rPred = pred 4 "r" SNat Logical

queryPred :: Predicate 0 ('ARename 'ABase)
queryPred = pred 5 "query" SNat Logical

p, q, s, a :: Int -> Term -> Literal ('ARename 'ABase)
p id t = lit id Positive pPred (fromJust $ V.fromList [ t ])
q id t = lit id Positive qPred (fromJust $ V.fromList [ t ])
s id t = lit id Positive sPred (fromJust $ V.fromList [ t ])
a id t = lit id Positive aPred (fromJust $ V.fromList [ t ])

r :: Int -> Term -> Term -> Literal ('ARename 'ABase)
r id t t' = lit id Positive rPred (fromJust $ V.fromList [ t, t' ])

query :: Int -> Literal ('ARename 'ABase)
query id = lit id Positive queryPred (fromJust $ V.fromList [ ])

flowSinkQ :: FlowSink 'ABase
flowSinkQ = FSinkLiteral (q 99 (tvar "X")) 0

flowSinkR :: FlowSink 'ABase
flowSinkR = FSinkLiteral (r 98 (tvar "X") (tvar "X")) 0

{-| Constant flow
-
- p(X) :- q(X).
- query() :- p(1).
|-}
prConst :: Program ('ARename 'ABase)
prConst = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30)        $ NE.fromList [ p 40 (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesConst :: Maybe [ FlowSource 'ABase ]
flowSourcesConst = Just [ FSourceConstant (CSym (symbol (1 :: Int))) ]

{-| Dead path
-
- Dead dataflow paths won't be evaluated, so we don't care if they lead to
- a dead end. Here only "query" is a query predicate.
-
- p(X) :- q(X).
- query() :- p(1).
- s(X) :- p(X).
|-}
prDeadPath :: Program ('ARename 'ABase)
prDeadPath = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30)        $ NE.fromList [ p 40 (tsym (1 :: Int)) ]
    , cl 300 (s 50 (tvar "X")) $ NE.fromList [ p 60 (tvar "X") ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesDeadPath :: Maybe [ FlowSource 'ABase ]
flowSourcesDeadPath = flowSourcesConst

{-| Dead exposed
-
- Same as constant flow ficture but p is exposed as a query predicate.
-
- p(X) :- q(X).
- query() :- p(1).
|-}
prExposed :: Program ('ARename 'ABase)
prExposed =
  prConst {queryPreds = [ PredicateBox queryPred, PredicateBox pPred ]}

flowSourcesExposed :: Maybe [ FlowSource 'ABase ]
flowSourcesExposed = Nothing

{-| Wildcard flow
-
- p(X) :- q(X).
- query() :- p(_).
|-}
prWild :: Program ('ARename 'ABase)
prWild = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30)        $ NE.fromList [ p 40 TWild ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesWild :: Maybe [ FlowSource 'ABase ]
flowSourcesWild = Just [ FSourceConstant CWild ]

{-| Single open
-
- p(X) :- q(X).
- query() :- p(X).
|-}
prSingleOpen :: Program ('ARename 'ABase)
prSingleOpen = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30)        $ NE.fromList [ p 40 (tvar "X") ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesSingleOpen :: Maybe [ FlowSource 'ABase ]
flowSourcesSingleOpen = Nothing

{-| Multiple closed
-
- p(X) :- q(X).
- query() :- a(X), p(X).
- query() :- p(1).
|-}
prMultipleClosed :: Program ('ARename 'ABase)
prMultipleClosed = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30) $ NE.fromList [ a 40 (tvar "X"), p 50 (tvar "X") ]
    , cl 300 (query 60) $ NE.fromList [ p 70 (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesMultipleClosed :: Maybe [ FlowSource 'ABase ]
flowSourcesMultipleClosed = Just
  [ FSourceLiteral (a 40 (tvar "X")) 0
  , FSourceConstant (CSym $ symbol (1 :: Int))
  ]

{-| Multiple half-open
-
- p(X) :- q(X).
- query() :- a(X), p(X).
- query() :- p(X).
|-}
prHalfOpen :: Program ('ARename 'ABase)
prHalfOpen = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30) $ NE.fromList [ a 40 (tvar "X"), p 50 (tvar "X") ]
    , cl 300 (query 60) $ NE.fromList [ p 70 (tvar "X") ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesHalfOpen :: Maybe [ FlowSource 'ABase ]
flowSourcesHalfOpen = Nothing

{-| Alias at head closed
-
- r(X,X) :- q(X).
- query() :- a(X), r(X,1).
|-}
prAliasHeadClosed :: Program ('ARename 'ABase)
prAliasHeadClosed = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (r 10 (tvar "X") (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30) $ NE.fromList [ a 40 (tvar "X"), r 50 (tvar "X") (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesAliasHeadClosed :: Maybe [ FlowSource 'ABase ]
flowSourcesAliasHeadClosed = Just
  [ FSourceConstant (CSym $ symbol (1 :: Int))
  , FSourceLiteral (a 40 (tvar "X")) 0
  ]

{-| Alias at head open
-
- r(X,X) :- q(X).
- query() :- r(X,1).
|-}
prAliasHeadOpen :: Program ('ARename 'ABase)
prAliasHeadOpen = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (r 10 (tvar "X") (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (query 30) $ NE.fromList [ r 40 (tvar "X") (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesAliasHeadOpen :: Maybe [ FlowSource 'ABase ]
flowSourcesAliasHeadOpen = Nothing

{-| Alias at body
-
- query() :- a(X), r(X,X).
|-}
prAliasBody :: Program ('ARename 'ABase)
prAliasBody = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (query 10) $ NE.fromList [ a 20 (tvar "X"), r 98 (tvar "X") (tvar "X") ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesAliasBody :: Maybe [ FlowSource 'ABase ]
flowSourcesAliasBody = Just [ FSourceLiteral (a 20 (tvar "X")) 0 ]

{-| Indirection
-
- s(X) :- q(X).
- p(X) :- s(X).
- query() :- p(1), s(2).
|-}
prIndirection :: Program ('ARename 'ABase)
prIndirection = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (s 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (p 30 (tvar "X")) $ NE.fromList [ s 40 (tvar "X") ]
    , cl 300 (query 50) $ NE.fromList [ p 60 (tsym (1 :: Int)), s 70 (tsym (2 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesIndirection :: Maybe [ FlowSource 'ABase ]
flowSourcesIndirection = Just
  [ FSourceConstant (CSym $ symbol (1 :: Int))
  , FSourceConstant (CSym $ symbol (2 :: Int))
  ]

{-| Recursion closed
-
- p(X) :- q(X).
- p(1) :- a(Y), p(Y).
- query() :- p(1).
|-}
prRecClosed :: Program ('ARename 'ABase)
prRecClosed = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (p 30 (tsym (1 :: Int))) $ NE.fromList [ a 40 (tvar "Y"), p 50 (tvar "Y") ]
    , cl 300 (query 60) $ NE.fromList [ p 70 (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesRecClosed :: Maybe [ FlowSource 'ABase ]
flowSourcesRecClosed = Just
  [ FSourceLiteral (a 40 (tvar "Y")) 0
  , FSourceConstant (CSym $ symbol (1 :: Int))
  ]

{-| Recursion closed but indifferent
-
- p(X) :- q(X).
- p(X) :- p(X).
- query() :- p(1).
|-}
prRecClosedIndiff :: Program ('ARename 'ABase)
prRecClosedIndiff = Program (ProgARename $ ProgABase dummySpan)
  [ [ cl 100 (p 10 (tvar "X")) $ NE.fromList [ q 99 (tvar "X") ]
    , cl 200 (p 30 (tvar "X")) $ NE.fromList [ p 40 (tvar "X") ]
    , cl 300 (query 50) $ NE.fromList [ p 60 (tsym (1 :: Int)) ]
    ]
  ] [ PredicateBox queryPred ]

flowSourcesRecClosedIndiff :: Maybe [ FlowSource 'ABase ]
flowSourcesRecClosedIndiff = Just
  [ FSourceConstant (CSym $ symbol (1 :: Int)) ]
