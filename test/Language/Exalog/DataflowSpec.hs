module Language.Exalog.DataflowSpec (spec) where

import Protolude

import Test.Hspec

import Data.Maybe (fromJust)

import qualified Fixture.RangeRestriction as RR
import qualified Fixture.Negation as Neg
import           Fixture.Util

import Language.Exalog.Core
import Language.Exalog.Dataflow
import Language.Exalog.Renamer
import Language.Exalog.Logger

edgeShouldExist :: (Show (f ann), Show (g ann), HasEdge f g ann)
                => PositiveFlowGr ann -> (f ann, Int) -> (g ann, Int)
                -> Expectation
edgeShouldExist flowGr src dst =
  shouldSatisfy (src, dst) (uncurry (isAnEdge flowGr))

edgeShouldntExist :: (Show (f ann), Show (g ann), HasEdge f g ann)
                => PositiveFlowGr ann -> (f ann, Int) -> (g ann, Int)
                -> Expectation
edgeShouldntExist flowGr src dst =
  shouldSatisfy (src, dst) (Protolude.not . uncurry (isAnEdge flowGr))

spec :: Spec
spec =
  describe "Dataflow" $
    describe "Positive" $ do
      it "programSimple has expected edges" $ do
        let gr = analysePositiveFlow RR.prSimple
        edgeShouldExist gr (RR.r (tvar "X"), 0) (PredicateBox RR.pPred, 0)
        edgeShouldExist gr (PredicateBox RR.queryPred, 0) (RR.r (tvar "X"), 0)

      renamedNegPr <- fromJust <$> (runIO . runLoggerT $ rename Neg.program)
      it "negation fixture has expected edges" $ do
        let gr = analysePositiveFlow renamedNegPr
        -- Clause 1
        edgeShouldExist gr (PredicateBox Neg.vPred, 0) (Neg.r (tvar "X") (tvar "Y"), 0)
        -- Clause 2
        edgeShouldExist gr (PredicateBox Neg.vPred, 0) (Neg.r (tvar "X") (tvar "Y"), 1)
        -- Clause 3
        edgeShouldExist gr (PredicateBox Neg.tPred, 0) (Neg.r (tvar "X") (tvar "Y"), 0)
        edgeShouldExist gr (PredicateBox Neg.tPred, 1) (Neg.r (tvar "X") (tvar "Y"), 1)
        -- Clause 4
        edgeShouldExist gr (PredicateBox Neg.tPred, 0) (Neg.t (tvar "X") (tvar "Z"), 0)
        edgeShouldExist gr (PredicateBox Neg.tPred, 1) (Neg.r (tvar "Z") (tvar "Y"), 1)
        edgeShouldExist gr (Neg.t (tvar "X") (tvar "Z"), 1) (Neg.r (tvar "Z") (tvar "Y"), 0)
        -- Following shouldn't exist because r is not intentional.
        edgeShouldntExist gr (Neg.t (tvar "X") (tvar "Z"), 1) (PredicateBox Neg.rPred, 0)
        -- Clause 5
        edgeShouldExist gr (PredicateBox Neg.tcPred, 0) (Neg.v (tvar "X"), 0)
        edgeShouldExist gr (PredicateBox Neg.tcPred, 1) (Neg.v (tvar "Y"), 0)
        edgeShouldExist gr (Neg.v (tvar "X"), 0) (Fixture.Util.not $ Neg.t (tvar "X") (tvar "Y"), 0)
        edgeShouldExist gr (Neg.v (tvar "Y"), 0) (Fixture.Util.not $ Neg.t (tvar "X") (tvar "Y"), 1)
        -- Following should exist because t is intentional.
        edgeShouldExist gr (Neg.v (tvar "X"), 0) (PredicateBox Neg.tPred, 0)
        edgeShouldExist gr (Neg.v (tvar "Y"), 0) (PredicateBox Neg.tPred, 1)
