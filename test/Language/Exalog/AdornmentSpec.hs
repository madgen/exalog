module Language.Exalog.AdornmentSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc

import Language.Exalog.Adornment

spec :: Spec
spec =
  describe "Adornment transformation" $
    parallel $ describe "Ancestor" $ do

      it "adorns linear ancestor correctly" $
        adornProgram LAnc.program `shouldBe` LAnc.adornedProgram

      it "adorns non-linear ancestor correctly" $
        adornProgram NLAnc.program `shouldBe` NLAnc.adornedProgram

      it "adorns swapped non-linear ancestor correctly" $
        adornProgram NLAnc.programSwapped `shouldBe` NLAnc.adornedProgramSwapped
