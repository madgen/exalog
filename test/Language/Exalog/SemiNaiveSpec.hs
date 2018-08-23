module Language.Exalog.SemiNaiveSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import Language.Exalog.SemiNaive

spec :: Spec
spec =
  describe "SemiNaive evaluation" $ do
    describe "Ancestor" $ do

      finalEDB <- runIO $ semiNaive AncEDB.initEDB LAnc.program
      it "evaluates linear ancestor correctly" $ do
        finalEDB `shouldBe` AncEDB.finalEDB
