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
  arbitrary = oneof $ return . Sym <$>
    [ "mistral", "emir", "nilufer", "laurent", "gulseren", "orhan"
    , "jean-pierre", "simone", "nazli", "hulusi" ]

instance (KnownNat n, SingI n) => Arbitrary (T.Tuples n) where
  arbitrary = do
    len <- arbitrary
    T.fromList <$> replicateM len (V.replicateM arbitrary :: Gen (V.Vector n Sym))

instance SingI n => Arbitrary (Predicate n 'ABase) where
  arbitrary = Predicate PredABase <$> arbitrary <*> pure (sing :: SNat n) <*> pure Logical

instance Arbitrary (R.Relation 'ABase) where
  arbitrary = do
    n <- oneof $ return <$> [1..10]
    withSomeSing n $
      \(snat :: SNat n) ->
        withKnownNat snat $
          R.Relation
            <$> (arbitrary :: Gen (Predicate n 'ABase))
            <*> (arbitrary :: Gen (T.Tuples n))

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
