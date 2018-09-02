module Language.Exalog.SolverSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Negation as NegFix

import qualified Language.Exalog.Relation as R
import           Language.Exalog.Solver

spec :: Spec
spec =
  describe "Solver " $ do
    finalEDB <- runIO $ solve NegFix.program NegFix.initEDB
    it "evaluates complement of a subgraph correctly" $
      R.findTuples finalEDB NegFix.tcPred `shouldBe` NegFix.tcTuples
