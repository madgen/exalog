{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.SolverSpec (spec, execSolver) where

import Protolude hiding (Set)

import Control.Monad (liftM2)

import qualified Data.Set as S

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
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Set as KB
import           Language.Exalog.Logger
import           Language.Exalog.Solver
import           Language.Exalog.Stratification

execSolver pr edb = runIO $ runLoggerT vanillaEnv $ do
  stratifiedPr <- stratify (decorate pr)
  solve stratifiedPr edb

infixr 1 `shouldBeish`
shouldBeish xs ys = S.fromList <$> xs `shouldBe` S.fromList <$> ys

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
          \(edb :: KB.Set 'ABase) -> unsafePerformIO $ liftM2 (==)
            (runLoggerT vanillaEnv $ evalSolver compute LAnc.program  edb)
            (runLoggerT vanillaEnv $ evalSolver compute NLAnc.program edb)

      finalEDB <- execSolver Const.program Const.initEDB
      it "evaluates constants correctly" $
        KB.findByPred Const.rPred <$> finalEDB `shouldBeish` Just Const.rTuples

      finalEDB <- execSolver Repeated.program Repeated.initEDB
      it "does not forget repeated variables" $
        KB.findByPred Repeated.pPred <$> finalEDB `shouldBeish` Just Repeated.pTuples

      finalEDB <- execSolver Wildcard.program Wildcard.initEDB
      it "evaluates literals with wildcads correctly" $
        KB.findByPred Wildcard.pPred <$> finalEDB `shouldBeish` Just Wildcard.pTuples

      describe "Foreign function" $ do

        finalEDB <- execSolver Foreign.programLeq100 Foreign.initLeq100EDB
        it "interprets 'x < 100' correctly" $
          KB.findByPred Foreign.leq100Pred <$> finalEDB `shouldBeish` Just Foreign.leq100Tuples

        finalEDB <- execSolver Foreign.programPrefixOf Foreign.initPrefixOfEDB
        it "interprets 'isPrefixOf' correctly" $
          KB.findByPred Foreign.prefixOfPred <$> finalEDB `shouldBeish` Just Foreign.prefixOfTuples

        finalEDB <- execSolver Foreign.programCartesian23 Foreign.initCartesian23EDB
        it "interprets 'cartesian23' correctly" $
          KB.findByPred Foreign.cartesian23Pred <$> finalEDB `shouldBeish` Just Foreign.cartesian23Tuples

        finalEDB <- execSolver Foreign.programImpure Foreign.initImpureEDB
        it "interprets 'impure' correctly" $
          KB.findByPred Foreign.impurePred <$> finalEDB `shouldBeish` Just Foreign.impureTuples

      finalEDB <- execSolver SpanIrr.program SpanIrr.initEDB
      it "evaluates correctly with different spans" $
        KB.findByPred SpanIrr.rPred <$> finalEDB `shouldBeish` Just SpanIrr.rTuples


-- Arbitrary instances for solution
instance Arbitrary Sym where
  arbitrary = oneof $ return . symbol <$>
    ([ "mistral", "emir", "nilufer", "laurent", "gulseren", "orhan"
    , "jean-pierre", "simone", "nazli", "hulusi" ] :: [ Text ])
