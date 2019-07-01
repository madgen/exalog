module Language.Exalog.DeltaSpec (spec) where

import Protolude

import Data.List ((!!))

import Test.Hspec

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc

import Language.Exalog.Core (Program(..))
import Language.Exalog.Delta

spec :: Spec
spec =
  describe "Delta analysis" $
    parallel $ describe "Ancestor" $ do

      it "deltaifies linear ancestor correctly" $
        mkDeltaStratum (strata LAnc.program !! 0) `shouldBe` LAnc.deltaStratum

      it "deltaifies non-linear ancestor correctly" $
        mkDeltaStratum (strata NLAnc.program !! 0) `shouldBe` NLAnc.deltaStratum
