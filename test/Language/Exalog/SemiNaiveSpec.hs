{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Language.Exalog.SemiNaiveSpec (spec) where

import Protolude hiding (Nat)

import           Data.String (fromString)
import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Data.Vector.Sized as V

import Control.Monad (liftM2, replicateM)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.IO.Unsafe (unsafePerformIO)

import qualified Fixture.Ancestor.LinearAncestor as LAnc
import qualified Fixture.Ancestor.NonLinearAncestor as NLAnc
import qualified Fixture.Ancestor.EDB as AncEDB

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SemiNaive

instance Arbitrary PredicateSym where
  arbitrary = fromString <$> arbitrary

instance Arbitrary Sym where
  arbitrary = Sym . fromString <$> arbitrary

instance Arbitrary (R.Relation 'ABase) where
  arbitrary = do
    n <- oneof $ return <$> [1..10]
    len <- arbitrary
    withSomeSing n $
      \(snat :: SNat n) ->
        withKnownNat snat $
          R.Relation
            <$> (Predicate PredABase <$> arbitrary <*> pure snat <*> pure Logical)
            <*> (T.fromList <$> replicateM len (V.replicateM arbitrary :: Gen (V.Vector n Sym)))

spec :: Spec
spec =
  describe "SemiNaive evaluation" $
    describe "Ancestor" $ do

      finalEDB <- runIO $ semiNaive AncEDB.initEDB LAnc.program
      it "evaluates linear ancestor correctly" $
        finalEDB `shouldBe` AncEDB.finalEDB

      finalEDB <- runIO $ semiNaive AncEDB.initEDB NLAnc.program
      it "evaluates non-linear ancestor correctly" $
        finalEDB `shouldBe` AncEDB.finalEDB

      prop "linear & non-linear versions produce the same result" $
        \edb -> unsafePerformIO $ liftM2 (==)
          (semiNaive edb LAnc.program)
          (semiNaive edb NLAnc.program)
