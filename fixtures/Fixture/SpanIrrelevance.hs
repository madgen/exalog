{-# LANGUAGE DataKinds #-}

module Fixture.SpanIrrelevance
  ( program
  , initEDB
  , rPred
  , rTuples
  ) where

import Protolude hiding (SrcLoc, Set)

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

cPred, rPred, rPred' :: Predicate 1 'ABase
cPred = Predicate (PredABase NoSpan) "c" SNat Logical
rPred = Predicate (PredABase NoSpan) "r" SNat Logical
rPred' = Predicate (PredABase (Span None (SrcLoc 1 2) (SrcLoc 2 3))) "r" SNat Logical

c,r,r' :: Term -> Literal 'ABase
c  t = lit cPred  $ fromJust $ V.fromList [ t ]
r  t = lit rPred  $ fromJust $ V.fromList [ t ]
r' t = lit rPred' $ fromJust $ V.fromList [ t ]

{-
- r("a") :- c("1").
- r("b") :- c("2").
-}
program :: Program 'ABase
program = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (r  (tsym ("a" :: Text))) $ NE.fromList [ c (tsym ("1" :: Text)) ]
      , Clause (ClABase NoSpan) (r' (tsym ("b" :: Text))) $ NE.fromList [ c (tsym ("2" :: Text)) ]
      ]
    ])
  [ PredicateBox rPred ]

cTuples :: [ V.Vector 1 Text ]
cTuples = fromJust . V.fromList <$>
  [ [ "1" ]
  , [ "2" ]
  ]

cKB :: Set 'ABase
cKB = fromList $ mkKnowledge cPred . fmap symbol <$> cTuples

initEDB :: Set 'ABase
initEDB = cKB

rTuples :: [ V.Vector 1 Sym ]
rTuples = fmap symbol . fromJust . V.fromList <$>
  ([ [ "a" ] , [ "b" ] ] :: [ [ Text ] ])
