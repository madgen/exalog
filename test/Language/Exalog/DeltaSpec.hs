module Language.Exalog.DeltaSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc

import Language.Exalog.Delta

spec :: Spec
spec =
  describe "Delta analysis" $
    parallel $ describe "Ancestor" $ do

      it "deltaifies linear ancestor correctly" $
        mkDeltaProgram LAnc.program `shouldBe` LAnc.deltaProgram

      it "deltaifies non-linear ancestor correctly" $
        mkDeltaProgram NLAnc.program `shouldBe` NLAnc.deltaProgram
