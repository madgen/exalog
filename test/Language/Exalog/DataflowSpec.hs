module Language.Exalog.DataflowSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.RangeRestriction as RR
import           Fixture.Util

import Language.Exalog.Core
import Language.Exalog.Dataflow
import Language.Exalog.Renamer ()

edgeShouldExist :: (Show (f ann), Show (g ann), HasEdge f g ann)
                => PositiveFlowGr ann -> (f ann, Int) -> (g ann, Int)
                -> Expectation
edgeShouldExist flowGr src dst =
  shouldSatisfy (src, dst) (uncurry (isAnEdge flowGr))

spec :: Spec
spec =
  describe "Dataflow" $
    describe "Positive" $ do
      it "programSimple has expected edges" $ do
        let gr = analysePositiveFlow RR.prSimple
        edgeShouldExist gr (RR.r (tvar "X"), 0) (PredicateBox RR.pPred, 0)
        edgeShouldExist gr (PredicateBox RR.queryPred, 0) (RR.r (tvar "X"), 0)
