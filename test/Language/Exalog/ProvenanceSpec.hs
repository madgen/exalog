{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Language.Exalog.ProvenanceSpec (spec) where

import Protolude hiding (head, Set)

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import Language.Exalog.Core
import Language.Exalog.SolverSpec (execSolver)
import Language.Exalog.Provenance ()
import Language.Exalog.KnowledgeBase.Set

spec :: Spec
spec =
  describe "Provenance recording" $
    describe "Ancestor" $ do
      let initEDB = decorate AncEDB.initEDB :: Set ('AProvenance 'ABase)

      finalEDB <- execSolver (decorate LAnc.program) initEDB
      it "records provenance for linear ancestor correctly" $
        finalEDB `shouldBe` Just AncEDB.finalLinearProvEDB

      finalEDB <- execSolver (decorate NLAnc.program) initEDB
      it "records provenance for non-linear ancestor correctly" $
        finalEDB `shouldBe` Just AncEDB.finalNonLinearProvEDB

      finalEDBL  <- execSolver (decorate LAnc.program)  initEDB
      finalEDBNL <- execSolver (decorate NLAnc.program) initEDB

      it "provenance of non-linear ancestor differs from provenance of linear ancestor" $
        finalEDBL `shouldNotBe` finalEDBNL
