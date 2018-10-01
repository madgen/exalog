{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Language.Exalog.SemiNaiveSpec (spec) where

import Protolude hiding (Nat)

import Control.Monad (liftM2)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.IO.Unsafe (unsafePerformIO)

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB
import qualified Fixture.Constant as Const
import qualified Fixture.Foreign as Foreign
import           Fixture.Util

import           Language.Exalog.Core hiding (Positive)
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SemiNaive

spec :: Spec
spec =
  describe "SemiNaive evaluation" $ do
    describe "Ancestor" $ do

      finalEDB <- runIO $ semiNaive LAnc.program AncEDB.initEDB
      it "evaluates linear ancestor correctly" $
        finalEDB `shouldBe` AncEDB.finalEDB

      finalEDB <- runIO $ semiNaive NLAnc.program AncEDB.initEDB
      it "evaluates non-linear ancestor correctly" $
        finalEDB `shouldBe` AncEDB.finalEDB

      prop "linear & non-linear versions produce the same result" $
        \edb -> unsafePerformIO $ liftM2 (==)
          (semiNaive LAnc.program edb)
          (semiNaive NLAnc.program edb)

    finalEDB <- runIO $ semiNaive Const.program Const.initEDB
    it "evaluates constants correctly" $
      R.findTuples finalEDB Const.rPred `shouldBe` Const.rTuples

    describe "Foreign function" $ do

      finalEDB <- runIO $ semiNaive Foreign.programLeq100 Foreign.initLeq100EDB
      it "interprets 'x < 100' correctly" $
        R.findTuples finalEDB Foreign.leq100Pred `shouldBe` Foreign.leq100Tuples

      finalEDB <- runIO $ semiNaive Foreign.programPrefixOf Foreign.initPrefixOfEDB
      it "interprets 'isPrefixOf' correctly" $
        R.findTuples finalEDB Foreign.prefixOfPred `shouldBe` Foreign.prefixOfTuples

      finalEDB <- runIO $ semiNaive Foreign.programCartesian23 Foreign.initCartesian23EDB
      it "interprets 'cartesian23' correctly" $
        R.findTuples finalEDB Foreign.cartesian23Pred `shouldBe` Foreign.cartesian23Tuples

-- Arbitrary instances for solution
instance Arbitrary Sym where
  arbitrary = oneof $ return . symbol <$>
    ([ "mistral", "emir", "nilufer", "laurent", "gulseren", "orhan"
    , "jean-pierre", "simone", "nazli", "hulusi" ] :: [ Text ])
