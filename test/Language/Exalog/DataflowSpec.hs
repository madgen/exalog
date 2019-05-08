{-# LANGUAGE DataKinds #-}

module Language.Exalog.DataflowSpec (spec) where

import Protolude hiding (not)

import Test.Hspec

import           Data.List ((!!))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)

import qualified Fixture.RangeRestriction as RR
import qualified Fixture.Negation as Neg
import qualified Fixture.Dataflow as DF
import           Fixture.Util

import Language.Exalog.Core hiding (head)
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
  shouldNotSatisfy (src, dst) (uncurry (isAnEdge flowGr))

findRenamedPred :: [ PredicateBox ('ARename 'ABase) ] -> PredicateBox 'ABase -> PredicateBox ('ARename 'ABase)
findRenamedPred pBoxes pBox = fromMaybe (panic "Predicate is not in the program.")
                            . head
                            . filter ((== pBox) . peel)
                            $ pBoxes

findRenamedLit :: [ Literal ('ARename 'ABase) ] -> Literal 'ABase -> Literal ('ARename 'ABase)
findRenamedLit lits literal = fromMaybe (panic "Literal is not in the program.")
                         . head
                         . filter ((== literal) . peel)
                         $ lits

spec :: Spec
spec =
  describe "Dataflow" $
    describe "Positive" $ do
      let rrGr = analysePositiveFlow RR.prSimple
      renamedNegPr <- fromJust <$> (runIO . runLoggerT $ rename Neg.program)
      let negGr = analysePositiveFlow renamedNegPr
      describe "Overall graph" $ do
        it "programSimple has expected edges" $ do
          edgeShouldExist rrGr (RR.r (tvar "X"), 0) (PredicateBox RR.pPred, 0)
          edgeShouldExist rrGr (PredicateBox RR.queryPred, 0) (RR.r (tvar "X"), 0)

        it "negation fixture has expected edges" $ do
          -- Clause 1
          edgeShouldExist negGr (PredicateBox Neg.vPred, 0) (Neg.r (tvar "X") (tvar "Y"), 0)
          -- Clause 2
          edgeShouldExist negGr (PredicateBox Neg.vPred, 0) (Neg.r (tvar "X") (tvar "Y"), 1)
          -- Clause 3
          edgeShouldExist negGr (PredicateBox Neg.tPred, 0) (Neg.r (tvar "X") (tvar "Y"), 0)
          edgeShouldExist negGr (PredicateBox Neg.tPred, 1) (Neg.r (tvar "X") (tvar "Y"), 1)
          -- Clause 4
          edgeShouldExist negGr (PredicateBox Neg.tPred, 0) (Neg.t (tvar "X") (tvar "Z"), 0)
          edgeShouldExist negGr (PredicateBox Neg.tPred, 1) (Neg.r (tvar "Z") (tvar "Y"), 1)
          edgeShouldExist negGr (Neg.t (tvar "X") (tvar "Z"), 1) (Neg.r (tvar "Z") (tvar "Y"), 0)
          -- Following shouldn't exist because r is not intentional.
          edgeShouldntExist negGr (Neg.t (tvar "X") (tvar "Z"), 1) (PredicateBox Neg.rPred, 0)
          -- Clause 5
          edgeShouldExist negGr (PredicateBox Neg.tcPred, 0) (Neg.v (tvar "X"), 0)
          edgeShouldExist negGr (PredicateBox Neg.tcPred, 1) (Neg.v (tvar "Y"), 0)
          edgeShouldExist negGr (Neg.v (tvar "X"), 0) (not $ Neg.t (tvar "X") (tvar "Y"), 0)
          edgeShouldExist negGr (Neg.v (tvar "Y"), 0) (not $ Neg.t (tvar "X") (tvar "Y"), 1)
          -- Following should exist because t is intentional.
          edgeShouldExist negGr (Neg.v (tvar "X"), 0) (PredicateBox Neg.tPred, 0)
          edgeShouldExist negGr (Neg.v (tvar "Y"), 0) (PredicateBox Neg.tPred, 1)

      describe "Nearest covering positives" $ do
        it "negation fixture has expected covers" $ do
          let prs = findRenamedPred $ predicates renamedNegPr
          let frs = (\cl -> findRenamedLit (NE.toList . literals $ cl)) <$> clauses renamedNegPr

          -- v(X) flows into t(X,Z)
          let sources1 = FSourceLiteral ((frs !! 4) (Neg.v (tvar "X"))) 0 NE.:| []
          let sink1    = FSinkLiteral ((frs !! 3) (Neg.t (tvar "X") (tvar "Z"))) 0
          nearestCoveringPositives negGr sink1 `shouldBe` Just sources1

          -- v(Y) doesn't flow into r(Z,Y) because the second argument of
          -- the t preceding r(Z,Y) is free.
          let sink2    = FSinkLiteral ((frs !! 3) (Neg.r (tvar "Z") (tvar "Y"))) 1
          nearestCoveringPositives negGr sink2 `shouldBe` Nothing

          -- There is no covering for the r(X,Y) in the first clause
          let sink3 = FSinkLiteral ((fromJust $ head frs) (Neg.r (tvar "X") (tvar "Y"))) 1
          nearestCoveringPositives negGr sink3 `shouldBe` Nothing

          -- There is no covering for tc predicate
          let sink4 = FSinkPredicate (prs (PredicateBox Neg.tcPred)) 1
          nearestCoveringPositives negGr sink4 `shouldBe` Nothing

        it "constant dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prConst
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesConst

        it "wildcard dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prWild
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesWild

        it "single open dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prSingleOpen
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesSingleOpen

        it "multiple closed dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prMultipleClosed
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesMultipleClosed

        it "half open dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prHalfOpen
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesHalfOpen

        it "alias head closed dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prAliasHeadClosed
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesAliasHeadClosed

        it "alias head open dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prAliasHeadOpen
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesAliasHeadOpen

        it "alias body dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prAliasBody
          nearestCoveringPositives flowGr DF.flowSinkR `shouldBe` DF.flowSourcesAliasBody

        it "indirection dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prIndirection
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesIndirection

        it "closed recursion dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prRecClosed
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesRecClosed

        it "indifferent closed recursion dataflow example has proper covers" $ do
          let flowGr = analysePositiveFlow DF.prRecClosedIndiff
          nearestCoveringPositives flowGr DF.flowSinkQ `shouldBe` DF.flowSourcesRecClosedIndiff
