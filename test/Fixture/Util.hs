{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fixture.Util
  ( tvar
  , tsym
  , symbol
  , lit
  , not
  ) where

import Protolude hiding (pred, not)

import           Data.String (fromString)
import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Data.Vector.Sized as V

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import qualified Test.QuickCheck.Modifiers as QM

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T
import qualified Language.Exalog.Relation as R

not :: Literal 'ABase -> Literal 'ABase
not l@Literal{polarity = pol} =
  l { polarity = if pol == Positive then Negative else Positive }

lit :: Predicate n 'ABase -> V.Vector n Term -> Literal 'ABase
lit = Literal LitABase Positive

-- |Smart constructor for terms
tvar :: Text -> Term
tvar = TVar . Var

class Termable a where
  symbol :: a -> Sym
  tsym   :: a -> Term
  tsym = TSym . symbol
  {-# MINIMAL symbol #-}

instance Termable Text where
  symbol = SymText

instance Termable Int where
  symbol = SymInt

instance Termable Float where
  symbol = SymFloat

instance Termable Bool where
  symbol = SymBool

-- Common and generic arbitrary instances

-- For Core
instance Arbitrary PredicateSym where
  arbitrary = fromString <$> arbitrary

instance SingI n => Arbitrary (Predicate n 'ABase) where
  arbitrary = Predicate PredABase <$> arbitrary <*> pure (sing :: SNat n) <*> pure Logical

-- For Relation
instance Arbitrary Sym => Arbitrary (R.Relation 'ABase) where
  arbitrary = do
    n <- oneof $ return <$> [1..10]
    withSomeSing n $
      \(snat :: SNat n) ->
        withKnownNat snat $
          R.Relation
            <$> (arbitrary :: Gen (Predicate n 'ABase))
            <*> (arbitrary :: Gen (T.Tuples n))

instance Arbitrary Sym => Arbitrary (R.Solution 'ABase) where
  arbitrary = R.fromList <$> arbitrary

-- For Tuples
instance (KnownNat n, SingI n, Arbitrary Sym) => Arbitrary (T.Tuples n) where
  arbitrary = do
    QM.Positive len <- arbitrary :: Gen (QM.Positive Int)
    T.fromList <$> replicateM len (V.replicateM arbitrary :: Gen (V.Vector n Sym))
