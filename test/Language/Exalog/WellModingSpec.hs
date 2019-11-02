module Language.Exalog.WellModingSpec (spec) where

import Protolude

import Test.Hspec

import qualified Fixture.WellModing as WM

import Language.Exalog.Logger
import Language.Exalog.WellModing

spec :: Spec
spec =
  describe "Well moding" $
    describe "Repair" $
      it "programSimple can be repaired" $ do
        let input = (WM.prSimple, mempty)
        let output = (WM.prSimpleRepaired, mempty)
        runLoggerT vanillaEnv (fixModing input) `shouldReturn` Just output
