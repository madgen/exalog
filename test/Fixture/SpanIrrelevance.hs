{-# LANGUAGE DataKinds #-}

module Fixture.SpanIrrelevance
  ( program
  , initEDB
  , rPred
  , rTuples
  ) where

import Protolude hiding (SrcLoc)

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation
import           Language.Exalog.SrcLoc

import Fixture.Util

cPred, rPred, rPred' :: Predicate 1 'ABase
cPred = Predicate (PredABase dummySpan) "c" SNat Logical
rPred = Predicate (PredABase dummySpan) "r" SNat Logical
rPred' = Predicate (PredABase (SrcSpan (SrcLoc "" 1 2) (SrcLoc "" 2 3))) "r" SNat Logical

c,r,r' :: Term -> Literal 'ABase
c  t = lit cPred  $ fromJust $ V.fromList [ t ]
r  t = lit rPred  $ fromJust $ V.fromList [ t ]
r' t = lit rPred' $ fromJust $ V.fromList [ t ]

{-
- r("a") :- c("1").
- r("b") :- c("2").
-}
program :: Program 'ABase
program = Program (ProgABase dummySpan)
  [ Clause (ClABase dummySpan) (r  (tsym ("a" :: Text))) $ NE.fromList [ c (tsym ("1" :: Text)) ]
  , Clause (ClABase dummySpan) (r' (tsym ("b" :: Text))) $ NE.fromList [ c (tsym ("2" :: Text)) ]
  ] []

cTuples :: [ V.Vector 1 Text ]
cTuples = fromJust . V.fromList <$>
  [ [ "1" ]
  , [ "2" ]
  ]

cRel :: Relation 'ABase
cRel = Relation cPred . T.fromList $ fmap symbol <$> cTuples

initEDB :: Solution 'ABase
initEDB = fromList [ cRel ]

rTuples :: T.Tuples 1
rTuples = T.fromList $ fmap symbol . fromJust . V.fromList <$>
  ([ [ "a" ] , [ "b" ] ] :: [ [ Text ] ])
