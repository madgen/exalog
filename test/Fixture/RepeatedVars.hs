{-# LANGUAGE DataKinds #-}

module Fixture.RepeatedVars
  ( program
  , initEDB
  , pPred
  , pTuples
  ) where

import Protolude

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation
import           Language.Exalog.SrcLoc (dummySpan)

import Fixture.Util

pPred :: Predicate 1 'ABase
pPred = Predicate (PredABase dummySpan) "p" SNat Logical

qPred :: Predicate 2 'ABase
qPred = Predicate (PredABase dummySpan) "q" SNat Logical

p :: Term -> Literal 'ABase
p t = lit pPred $ fromJust $ V.fromList [ t ]

q :: Term -> Term -> Literal 'ABase
q t t' = lit qPred $ fromJust $ V.fromList [ t, t' ]

{-| Repeated variable program
-
- p(X) :- q(X,X).
|-}
program :: Program 'ABase
program = Program (ProgABase dummySpan)
  [ [ Clause (ClABase dummySpan) (p (tvar "X")) $ NE.fromList [ q (tvar "X") (tvar "X") ]
    ]
  ] [ PredicateBox pPred ]

qTuples :: [ V.Vector 2 Int ]
qTuples = fromJust . V.fromList <$>
  [ [ 1     , 2 ]
  , [ 2     , 2 ]
  ]

qRel :: Relation 'ABase
qRel = Relation qPred . T.fromList $ fmap symbol <$> qTuples

initEDB :: Solution 'ABase
initEDB = fromList [ qRel ]

pTuples :: T.Tuples 1
pTuples = T.fromList $ fmap symbol . fromJust . V.fromList <$>
  ([ [ 2 ] ] :: [ [ Int ] ])
