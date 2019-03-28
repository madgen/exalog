module Language.Exalog.SolverSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Negation as NegFix

import Language.Exalog.Logger
import Language.Exalog.Solver

spec :: Spec
spec =
  describe "Solver " $ do
    finalEDB <- runIO $ runLoggerT $ solve NegFix.program NegFix.initEDB
    it "evaluates complement of a subgraph correctly" $
      finalEDB `shouldBe` Just NegFix.finalEDB
