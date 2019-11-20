{-# LANGUAGE DataKinds #-}

module Language.Exalog.WellModingSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.WellModing as WM

import           Language.Exalog.Annotation
import           Language.Exalog.Logger
import           Language.Exalog.WellModing
import qualified Language.Exalog.KnowledgeBase.Set as KB

spec :: Spec
spec =
  describe "Well moding" $
    describe "Repair" $
      it "programSimple can be repaired" $ do
        let input = (WM.prSimple, mempty :: KB.Set ('ARename 'ABase))
        let output = (WM.prSimpleRepaired, mempty :: KB.Set 'ABase)
        runLoggerT vanillaEnv (fixModing input) `shouldReturn` Just output
