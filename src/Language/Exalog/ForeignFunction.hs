{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin-opt GHC.TypeLits.Normalise:assert-constants-natural #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.ForeignFunction
  ( liftPredicate
  , liftPredicateME
  , liftFunction
  , liftFunctionME
  ) where

import Protolude hiding (TypeError)

import           Data.Type.Bool (If)
import           Data.Text (unpack, pack)
import qualified Data.Vector.Sized as V

import Text.Read (readMaybe)

import GHC.TypeLits as TL (type (+), type (<=))

import Language.Exalog.Core

--------------------------------------------------------------------------------
-- Lift functions that return Bool
--------------------------------------------------------------------------------

{- | Lifts Haskell functions that returns Bool to 'ForeignFunc' to back
- extralogical predicates.
-
- For example, if an extralogical predicate @p@ is backed by a Haskell
- function of type @'Int' -> 'Char' -> 'Bool'@, a subgoal involving @p@
- would be of the form @p(I,C)@, where I and C are 'Text' representations
- of inhabitants of 'Int' and 'Char' respectively.
-
- While evaluating @p(I,C)@, the variables have to be bound or are
- constants, or it is an error. This subgoal holds when the Haskell
- function backing them returns 'True' in which case we return a singleton
- answer set. Otherwise, it returns an empty answer set.
-}
liftPredicate :: (Applicable f, RetTy f ~ Bool) => f -> ForeignFunc (Arity f)
liftPredicate p v = return . return $ [ fromTerm <$> v | p @@ v ]

{- | A variant of 'liftPredicate' for functions that have side effects and
- may produce errors.
-}
liftPredicateME :: (Applicable f, RetTy f ~ IO (Either Text Bool))
                => f -> ForeignFunc (Arity f)
liftPredicateME p v =
  fmap (\cond -> [ fromTerm <$> v | cond ]) <$> p @@ v

--------------------------------------------------------------------------------
-- Lift functions that do not return Bool
--------------------------------------------------------------------------------

{- | Lifts Haskell functions to 'ForeignFunc' to back extralogical predicates.
-
- For example, if an extralogical predicate @p@ is backed by a Haskell
- function of type @'Int' -> 'Char' -> [ 'Int' ]@, a subgoal involving @p@
- would be of the form @p(I1,C,I2)@, where @I1@, @C@, @I2@ are 'Text'
- representations of inhabitants of 'Int', 'Char' and 'Int' respectively.
-
- While evaluating @p(I1,C,I2)@, the variables @I1@ and @C@ have to be
- bound or are constants, or it is an error. Variable @I2@ may or may not
- be bound or ground. This subgoal holds when the Haskell function backing
- it returns a value of type @[ 'Int' ]@ and if @IS@ happens to be bound,
- its value needs to be in the returned list of integers. Otherwise, it
- returns an empty answer set.
-}
liftFunction :: (Applicable f, RetTy f ~ r, Returnable r, KnownNat (Arity f))
             => f -> ForeignFunc (NRets r + Arity f)
liftFunction f v = return . return $ genTuples (toReturnVs $ f @@ v) v

{- | A variant of 'liftFunction' for functions that have side effects and
- may produce errors.
-}
liftFunctionME :: (Applicable f, RetTy f ~ IO (Either Text r), Returnable r, KnownNat (Arity f))
               => f -> ForeignFunc (NRets r + Arity f)
liftFunctionME f v = do
  eResss <- fmap toReturnVs <$> f @@ v
  return $ do
    resss <- eResss
    return $ genTuples resss v

genTuples :: forall na nr
           . KnownNat na
          => [ V.Vector nr Sym ]
          -> V.Vector (na + nr) Term
          -> [ V.Vector (na + nr) Sym ]
genTuples resss v =
  [ symArgs V.++ ress
  | ress <- filterFakeResults rets resss ]
  where
  symArgs = fromTerm <$> args
  (args, rets) = V.splitAt @na v

-- Eliminate tuples with results that contradict with what is bound in the
-- subgoal for that result.
filterFakeResults :: V.Vector nr Term
                  -> [ V.Vector nr Sym ]
                  -> [ V.Vector nr Sym ]
filterFakeResults terms =
  filter (\ress -> all (uncurry consistent) $ V.zip ress terms)

-- Check if a particular result is consistent with the given term
consistent :: Sym -> Term -> Bool
consistent sym = \case
  TSym sym' -> sym == sym'
  TVar{}  -> True

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

type family NRets a :: Nat where
  NRets Text  = 1
  NRets Int   = 1
  NRets Float = 1
  NRets Bool  = 1
  NRets (a,b) = NRets a + NRets b
  NRets (a,b,c) = NRets a + NRets b + NRets c
  NRets (a,b,c,d) = NRets a + NRets b + NRets c + NRets d
  NRets [ a ] = NRets a

class Returnable' r where
  toReturnV :: r -> V.Vector (NRets r) Sym

instance Returnable' Text where
  toReturnV t = V.singleton (SymText t)

instance Returnable' Int where
  toReturnV i = V.singleton (SymInt i)

instance Returnable' Float where
  toReturnV f = V.singleton (SymFloat f)

instance Returnable' Bool where
  toReturnV b = V.singleton (SymBool b)

instance (Returnable' a, Returnable' b) => Returnable' (a,b) where
  toReturnV (a,b) = toReturnV a V.++ toReturnV b

instance (Returnable' a, Returnable' b, Returnable' c)
      => Returnable' (a,b,c) where
  toReturnV (a,b,c) = toReturnV a V.++ toReturnV b V.++ toReturnV c

instance (Returnable' a, Returnable' b, Returnable' c, Returnable' d)
      => Returnable' (a,b,c,d) where
  toReturnV (a,b,c,d) =
    toReturnV a V.++ toReturnV b V.++ toReturnV c V.++ toReturnV d

class Returnable r where
  toReturnVs :: r -> [ V.Vector (NRets r) Sym ]

instance Returnable' a => Returnable a where
  toReturnVs x = [ toReturnV x ]

instance Returnable' a => Returnable [ a ] where
  toReturnVs xs = map toReturnV xs

type family Ground a where
  Ground Text   = 'True
  Ground Int    = 'True
  Ground Float  = 'True
  Ground Bool   = 'True
  Ground _      = 'False

class Argumentable a where
  interpret :: Sym -> a

instance Argumentable Text where
  interpret (SymText t) = t
  interpret _ =
    panic "Fatal error: Foreign function was expecting arugment of type Text."

instance Argumentable Int where
  interpret (SymInt i) = i
  interpret _ =
    panic "Fatal error: Foreign function was expecting arugment of type Int."

instance Argumentable Float where
  interpret (SymFloat f) = f
  interpret _ =
    panic "Fatal error: Foreign function was expecting arugment of type Char."

instance Argumentable Bool where
  interpret (SymBool b) = b
  interpret _ =
    panic "Fatal error: Foreign function was expecting arugment of type Bool."

type family RetTy f where
  RetTy (a -> r) = If (Ground r) r (RetTy r)

type family Arity f :: Nat where
  Arity (a -> r) = If (Ground r) 1 (Arity r + 1)
  Arity r = 1

type Applicable f = Applicable' f (Arity f)

class ari ~ Arity f => Applicable' f (ari :: Nat) where
  (@@) :: Arity f <= n => f -> V.Vector n Term -> RetTy f

instance ( Ground r ~ 'True
         , Argumentable a
         ) => Applicable' (a -> r) 1 where
  f @@ v | [ arg ] <- take 1 . V.toList $ v = f (interpret . fromTerm $ arg)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b
         ) => Applicable' (a -> b -> r) 2 where
  f @@ v | [ arg1, arg2 ] <- fromTerm <$> (take 2 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c
         ) => Applicable' (a -> b -> c -> r) 3 where
  f @@ v | [ arg1, arg2, arg3 ] <- fromTerm <$> (take 3 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c, Argumentable d
         ) => Applicable' (a -> b -> c -> d -> r) 4 where
  f @@ v | [ arg1, arg2, arg3, arg4 ] <- fromTerm <$> (take 4 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)
      (interpret arg4)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c, Argumentable d, Argumentable e
         ) => Applicable' (a -> b -> c -> d -> e -> r) 5 where
  f @@ v | [ arg1, arg2, arg3, arg4, arg5 ] <- fromTerm <$> (take 5 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)
      (interpret arg4)
      (interpret arg5)

fromTerm :: Term -> Sym
fromTerm = \case
  TSym s -> s
  TVar{} -> panic
    "Mode error: Foreign function argument is not sufficiently bound."
