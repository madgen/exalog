{-# LANGUAGE DataKinds #-}

module Fixture.Wildcard
  ( program
  , initEDB
  , pPred
  , pTuples
  ) where

import Protolude hiding (not, Set)

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.KnowledgeBase.Class
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))

import Fixture.Util

pPred :: Predicate 1 'ABase
pPred = Predicate (PredABase NoSpan) "p" SNat Logical

qPred :: Predicate 2 'ABase
qPred = Predicate (PredABase NoSpan) "q" SNat Logical

p :: Term -> Literal 'ABase
p t = lit pPred $ fromJust $ V.fromList [ t ]

q :: Term -> Term -> Literal 'ABase
q t t' = lit qPred $ fromJust $ V.fromList [ t, t' ]

{-| Repeated variable program
-
- p(1) :- q(_,_).
- p(X) :- q(X,_), ! q(_,X).
|-}
program :: Program 'ABase
program = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (p (tsym (1 :: Int))) $ NE.fromList [ q TWild TWild ]
      , Clause (ClABase NoSpan) (p (tvar "X"))
        $ NE.fromList
          [ q (tvar "X") TWild
          , not $ q TWild      (tvar "X")]
      ]
    ])
  [ PredicateBox pPred ]

qTuples :: [ V.Vector 2 Int ]
qTuples = fromJust . V.fromList <$>
  [ [ 1     , 2 ]
  , [ 2     , 2 ]
  , [ 2     , 1 ]
  , [ 3     , 1 ]
  , [ 4     , 3 ]
  , [ 5     , 6 ]
  ]

qKB :: Set 'ABase
qKB = fromList $ Knowledge qPred . fmap symbol <$> qTuples

initEDB :: Set 'ABase
initEDB = qKB

pTuples :: [ V.Vector 1 Sym ]
pTuples = fmap symbol . fromJust . V.fromList <$>
  ([ [ 1 ], [ 4 ], [ 5 ] ] :: [ [ Int ] ])
