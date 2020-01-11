{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Language.Exalog.ProvenanceSpec (spec) where

import Protolude hiding (head)

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import Language.Exalog.Core
import Language.Exalog.SolverSpec (execSolver)
import Language.Exalog.Provenance ()
import Language.Exalog.KnowledgeBase.Class
import Language.Exalog.KnowledgeBase.Knowledge

decorateGiven :: IdentifiableAnn (Ann (Predicate n) a) id
              => IdentifiableAnn (Ann Literal a) id'
              => IdentifiableAnn (Ann Clause a) id''
              => IdentifiableAnn (Ann Knowledge a) id'''
              => Ord id => Ord id' => Ord id'' => Ord id'''
              => Knowledgeable sol a
              => sol a -> sol ('AProvenance a)
decorateGiven = atEach decorate

spec :: Spec
spec =
  describe "Provenance recording" $
    parallel $ describe "Ancestor" $ do

      finalEDB <- execSolver (decorate LAnc.program) (decorateGiven AncEDB.initEDB)
      it "records provenance for linear ancestor correctly" $
        finalEDB `shouldBe` Just AncEDB.finalLinearProvEDB

      finalEDB <- execSolver (decorate NLAnc.program) (decorateGiven AncEDB.initEDB)
      it "records provenance for non-linear ancestor correctly" $
        finalEDB `shouldBe` Just AncEDB.finalNonLinearProvEDB

      finalEDBL  <- execSolver (decorate LAnc.program)  (decorateGiven AncEDB.initEDB)
      finalEDBNL <- execSolver (decorate NLAnc.program) (decorateGiven AncEDB.initEDB)
      it "provenance of non-linear ancestor differs from provenance of linear ancestor" $
        finalEDBL `shouldNotBe` finalEDBNL
