{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Set as KBS
import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))

not :: Literal 'ABase -> Literal 'ABase
not l@Literal{_polarity = pol} =
  l {_polarity = if pol == Positive then Negative else Positive}

lit :: Predicate n 'ABase -> V.Vector n Term -> Literal 'ABase
lit = Literal (LitABase NoSpan) Positive

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

instance Termable Bool where
  symbol = SymBool

-- Common and generic arbitrary instances

-- For Core
instance Arbitrary PredicateSymbol where
  arbitrary = fromString <$> arbitrary

instance SingI n => Arbitrary (Predicate n 'ABase) where
  arbitrary = Predicate (PredABase NoSpan)
          <$> arbitrary
          <*> pure (sing :: SNat n)
          <*> pure Logical

instance Arbitrary Sym => Arbitrary (KB.Knowledge 'ABase) where
  arbitrary = do
    n <- oneof $ return <$> [1..10]
    withSomeSing n $
      \(snat :: SNat n) ->
        withKnownNat snat $
          KB.Knowledge KnowABase
            <$> (arbitrary :: Gen (Predicate n 'ABase))
            <*> (arbitrary :: Gen (V.Vector n Sym))

instance (KnownNat n, Arbitrary a) => Arbitrary (V.Vector n a) where
  arbitrary = V.replicateM arbitrary

instance Arbitrary Sym => Arbitrary (KBS.Set 'ABase) where
  arbitrary = KB.fromList <$> arbitrary
