{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Exalog.AdornmentSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import Language.Exalog.Adornment
import Language.Exalog.Core (decorate)
import Language.Exalog.Relation

import Language.Exalog.SolverSpec (execSolver)

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

      finalEDB <- execSolver NLAnc.adornedProgram (rename decorate AncEDB.initEDB)
      it "adornment preserves non-linear ancestor solutions" $
        finalEDB `shouldBe` Just (rename decorate AncEDB.finalEDB)

      finalEDB <- execSolver LAnc.adornedProgram (rename decorate AncEDB.initEDB)
      it "adornment preserves linear ancestor solutions" $
        finalEDB `shouldBe` Just (rename decorate AncEDB.finalEDB)
