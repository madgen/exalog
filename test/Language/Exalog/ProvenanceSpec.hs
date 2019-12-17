{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Language.Exalog.ProvenanceSpec (spec) where

import Protolude hiding (head)

import Data.List (head)

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import Language.Exalog.SolverSpec (execSolver)
import Language.Exalog.Core (Program(..), decorate, type Decored)
import Language.Exalog.Provenance
import Language.Exalog.KnowledgeBase.Class
import Language.Exalog.KnowledgeBase.Knowledge

spec :: Spec
spec =
    describe "Provenance recording" $
    parallel $ describe "Ancestor" $ do
    
        finalEDB <- execSolver (decorate LAnc.program) (atEach (\(Knowledge ann pred syms) -> Knowledge (KnowAProvenance Given ann) (decorate pred) syms) AncEDB.initEDB)
        it "records provenance for linear ancestor correctly" $
            finalEDB `shouldBe` Just AncEDB.finalLinearProvEDB

        finalEDB <- execSolver (decorate NLAnc.program) (atEach (\(Knowledge ann pred syms) -> Knowledge (KnowAProvenance Given ann) (decorate pred) syms) AncEDB.initEDB)
        it "records provenance for non-linear ancestor correctly" $
            finalEDB `shouldBe` Just AncEDB.finalNonLinearProvEDB

        finalEDBL  <- execSolver (decorate LAnc.program)  (atEach (\(Knowledge ann pred syms) -> Knowledge (KnowAProvenance Given ann) (decorate pred) syms) AncEDB.initEDB)
        finalEDBNL <- execSolver (decorate NLAnc.program) (atEach (\(Knowledge ann pred syms) -> Knowledge (KnowAProvenance Given ann) (decorate pred) syms) AncEDB.initEDB)
        it "provenance of non-linear ancestor differs from provenance of linear ancestor" $
            (finalEDBL) `shouldNotBe` (finalEDBNL)
    