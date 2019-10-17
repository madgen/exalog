{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.SolverSpec (spec, execSolver) where

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
import qualified Fixture.Negation as NegFix
import qualified Fixture.RepeatedVars as Repeated
import qualified Fixture.SpanIrrelevance as SpanIrr
import           Fixture.Util
import qualified Fixture.Wildcard as Wildcard

import           Language.Exalog.Core hiding (Positive)
import           Language.Exalog.Dependency ()
import           Language.Exalog.Logger
import qualified Language.Exalog.Relation as R
import           Language.Exalog.Solver
import           Language.Exalog.Stratification

execSolver pr edb = runIO $ runLoggerT $ do
  stratifiedPr <- stratify (decorate pr)
  solve stratifiedPr edb

spec :: Spec
spec =
  describe "Solver " $ do
    finalEDB <- execSolver NegFix.program NegFix.initEDB
    it "evaluates complement of a subgraph correctly" $
      finalEDB `shouldBe` Just NegFix.finalEDB

    describe "SemiNaive evaluation" $ do
      describe "Ancestor" $ do

        finalEDB <- execSolver LAnc.program AncEDB.initEDB
        it "evaluates linear ancestor correctly" $
          finalEDB `shouldBe` Just AncEDB.finalEDB

        finalEDB <- execSolver NLAnc.program AncEDB.initEDB
        it "evaluates non-linear ancestor correctly" $
          finalEDB `shouldBe` Just AncEDB.finalEDB

        prop "linear & non-linear versions produce the same result" $
          \edb -> unsafePerformIO $ liftM2 (==)
            (runLoggerT $ evalSolver compute LAnc.program  edb)
            (runLoggerT $ evalSolver compute NLAnc.program edb)

      finalEDB <- execSolver Const.program Const.initEDB
      it "evaluates constants correctly" $
        R.findTuplesByPred Const.rPred <$> finalEDB `shouldBe` Just Const.rTuples

      finalEDB <- execSolver Repeated.program Repeated.initEDB
      it "does not forget repeated variables" $
        R.findTuplesByPred Repeated.pPred <$> finalEDB `shouldBe` Just Repeated.pTuples

      finalEDB <- execSolver Wildcard.program Wildcard.initEDB
      it "evaluates literals with wildcads correctly" $
        R.findTuplesByPred Wildcard.pPred <$> finalEDB `shouldBe` Just Wildcard.pTuples

      describe "Foreign function" $ do

        finalEDB <- execSolver Foreign.programLeq100 Foreign.initLeq100EDB
        it "interprets 'x < 100' correctly" $
          R.findTuplesByPred Foreign.leq100Pred <$> finalEDB `shouldBe` Just Foreign.leq100Tuples

        finalEDB <- execSolver Foreign.programPrefixOf Foreign.initPrefixOfEDB
        it "interprets 'isPrefixOf' correctly" $
          R.findTuplesByPred Foreign.prefixOfPred <$> finalEDB `shouldBe` Just Foreign.prefixOfTuples

        finalEDB <- execSolver Foreign.programCartesian23 Foreign.initCartesian23EDB
        it "interprets 'cartesian23' correctly" $
          R.findTuplesByPred Foreign.cartesian23Pred <$> finalEDB `shouldBe` Just Foreign.cartesian23Tuples

      finalEDB <- execSolver SpanIrr.program SpanIrr.initEDB
      it "evaluates correctly with different spans" $
        R.findTuplesByPred SpanIrr.rPred <$> finalEDB `shouldBe` Just SpanIrr.rTuples


-- Arbitrary instances for solution
instance Arbitrary Sym where
  arbitrary = oneof $ return . symbol <$>
    ([ "mistral", "emir", "nilufer", "laurent", "gulseren", "orhan"
    , "jean-pierre", "simone", "nazli", "hulusi" ] :: [ Text ])
