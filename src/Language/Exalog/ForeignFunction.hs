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

data BaseTy = TText Text | TInt Int | TChar Char

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
liftFunction f v = return . return $ genTuples (toBaseVector $ f @@ v) v

{- | A variant of 'liftFunction' for functions that have side effects and
- may produce errors.
-}
liftFunctionME :: (Applicable f, RetTy f ~ IO (Either Text r), Returnable r, KnownNat (Arity f))
               => f -> ForeignFunc (NRets r + Arity f)
liftFunctionME f v = do
  eResss <- fmap toBaseVector <$> f @@ v
  return $ do
    resss <- eResss
    return $ genTuples resss v

genTuples :: forall na nr
           . KnownNat na
          => [ V.Vector nr BaseTy ]
          -> V.Vector (na + nr) Term
          -> [ V.Vector (na + nr) Sym ]
genTuples resss v =
  [ symArgs V.++ (toSym <$> ress)
  | ress <- filterFakeResults rets resss ]
  where
  symArgs = fromTerm <$> args
  (args, rets) = V.splitAt @na v

-- Eliminate tuples with results that contradict with what is bound in the
-- subgoal for that result.
filterFakeResults :: V.Vector nr Term
                  -> [ V.Vector nr BaseTy ]
                  -> [ V.Vector nr BaseTy ]
filterFakeResults terms =
  filter (\ress -> all (uncurry consistent) $ V.zip ress terms)

-- Check if a particular result is consistent with the given term
consistent :: BaseTy -> Term -> Bool
consistent a = \case
  TSym (Sym text) ->
    case a of
      TInt i  -> i == interpret text
      TChar c -> c == interpret text
      TText t -> t == interpret text
  TVar{}  -> True

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

type family NRets a :: Nat where
  NRets Int  = 1
  NRets Char = 1
  NRets Text = 1
  NRets [ a ] = NRets a
  NRets (a,b) = NRets a + NRets b

class Returnable r where
  toBaseVector :: r -> [ V.Vector (NRets r) BaseTy ]

instance Returnable Int where
  toBaseVector i = [ V.singleton (TInt i) ]

instance Returnable Char where
  toBaseVector c = [ V.singleton (TChar c) ]

instance Returnable Text where
  toBaseVector t = [ V.singleton (TText t) ]

instance Returnable a => Returnable [ a ] where
  toBaseVector xs = join . map toBaseVector $ xs

instance (Returnable a, Returnable b) => Returnable (a,b) where
  toBaseVector (a, b) = zipWith (V.++) (toBaseVector a) (toBaseVector b)

type family Ground a where
  Ground Text   = 'True
  Ground Char   = 'True
  Ground Bool   = 'True
  Ground Int    = 'True
  Ground _      = 'False

class Argumentable a where
  interpret :: Text -> a

instance Argumentable Text where
  interpret x = x

instance Argumentable Char where
  interpret text
    | Just c <- readMaybe . unpack $ text = c
    | otherwise =
      panic $ "Fatal error: '" <> text <> "' couldn't be interpeted as a Char."

instance Argumentable Bool where
  interpret text
    | Just b <- readMaybe . unpack $ text = b
    | otherwise =
      panic $ "Fatal error: '" <> text <> "' couldn't be interpeted as a Bool."

instance Argumentable Int where
  interpret text
    | Just i <- readMaybe . unpack $ text = i
    | otherwise =
      panic $ "Fatal error: '" <> text <> "' couldn't be interpeted as a Int."

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
  f @@ v | [ arg ] <- termToText <$> (take 1 . V.toList) v = f (interpret arg)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b
         ) => Applicable' (a -> b -> r) 2 where
  f @@ v | [ arg1, arg2 ] <- termToText <$> (take 2 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c
         ) => Applicable' (a -> b -> c -> r) 3 where
  f @@ v | [ arg1, arg2, arg3 ] <- termToText <$> (take 3 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c, Argumentable d
         ) => Applicable' (a -> b -> c -> d -> r) 4 where
  f @@ v | [ arg1, arg2, arg3, arg4 ] <- termToText <$> (take 4 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)
      (interpret arg4)

instance ( Ground r ~ 'True
         , Argumentable a, Argumentable b, Argumentable c, Argumentable d, Argumentable e
         ) => Applicable' (a -> b -> c -> d -> e -> r) 5 where
  f @@ v | [ arg1, arg2, arg3, arg4, arg5 ] <- termToText <$> (take 5 . V.toList) v =
    f (interpret arg1)
      (interpret arg2)
      (interpret arg3)
      (interpret arg4)
      (interpret arg5)

toSym :: BaseTy -> Sym
toSym (TText text) = Sym text
toSym (TInt i) = Sym . pack . show $ i
toSym (TChar c) = Sym . pack . show $ c

termToText :: Term -> Text
termToText (TSym (Sym text)) = text
termToText TVar{} = panic
  "Fatal error: Foreign function argument is not sufficiently bound."

fromTerm :: Term -> Sym
fromTerm = \case
  TSym s -> s
  TVar{} -> panic
    "Mode error: Foreign function argument is not sufficiently bound."
