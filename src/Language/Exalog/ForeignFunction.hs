{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.ForeignFunction
  ( liftPredicate
  , liftPredicateME
  , liftFunction
  , liftFunctionME
  ) where

import Protolude

import           Data.Type.Bool (type If)
import           Data.Text (unpack, pack)
import qualified Data.Vector.Sized as V

import Text.Read (read)

import GHC.TypeLits (type (+))

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
liftPredicate p v = return . return $ [ symbols | p @@ symbols ]
  where
  symbols = fromTerm <$> v

{- | A variant of 'liftPredicate' for functions that have side effects and
- may produce errors.
-}
liftPredicateME :: (Applicable f, RetTy f ~ IO (Either Text Bool))
                => f -> ForeignFunc (Arity f)
liftPredicateME p v =
  fmap (\cond -> [ symbols | cond ]) <$> p @@ symbols
  where
  symbols = fromTerm <$> v

--------------------------------------------------------------------------------
-- Lift functions that do not return Bool
--------------------------------------------------------------------------------

{- | Lifts Haskell functions to 'ForeignFunc' to back extralogical predicates.
-
- For example, if an extralogical predicate @p@ is backed by a Haskell
- function of type @'Int' -> 'Char' -> 'String'@, a subgoal involving @p@
- would be of the form @p(S,I,C)@, where @S@, @I@ and @C@ are 'Text'
- representations of inhabitants of 'String', 'Int' and 'Char respectively.
-
- While evaluating @p(S,I,C)@, the variables @I@ and @C@ have to be bound or are
- constants, or it is an error. Variable @S@ may or may not be bound or
- ground. This subgoal holds when the Haskell function backing it returns
- a value of type 'String' which and if @S@ happens to be bound, this value
- is compared to the value of @S@. Otherwise, it returns an empty answer set.
-}
liftFunction :: (Applicable f, RetTy f ~ b, Show b, Read b, Eq b)
             => f -> ForeignFunc (1 + Arity f)
liftFunction f v = return . return $
  [ V.cons (toSym res) tailSymbols | res `consistent` V.head v ]
  where
  res = f @@ tailSymbols
  tailSymbols = fromTerm <$> V.tail v

{- | A variant of 'liftFunction' for functions that have side effects and
- may produce errors.
-}
liftFunctionME :: (Applicable f, RetTy f ~ IO (Either Text b), Show b, Read b, Eq b)
               => f -> ForeignFunc (1 + Arity f)
liftFunctionME f v = do
  eRes <- f @@ tailSymbols
  return $ do
    res <- eRes
    return [ V.cons (toSym res) tailSymbols | res `consistent` V.head v ]
  where
  tailSymbols = fromTerm <$> V.tail v

consistent :: (Eq a, Read a) => a -> Term -> Bool
consistent a = \case
  TSym (Sym text) -> a == read (unpack text)
  TVar{}  -> True

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

type family Ground a where
  Ground Bool   = 'True
  Ground Char   = 'True
  Ground Int    = 'True
  Ground _      = 'False

type family RetTy f where
  RetTy (a -> r) = If (Ground r) r (RetTy r)

type family Arity f :: Nat where
  Arity (a -> r) = If (Ground r) 1 (Arity r + 1)
  Arity r = 1

class Applicable f where
  (@@) :: f -> V.Vector (Arity f) Sym -> RetTy f

instance ( Ground r ~ 'True
         , Read a
         ) => Applicable (a -> r) where
  f @@ v | [ arg ] <- symToStr <$> V.toList v = f (read arg)

instance ( Ground r ~ 'True
         , Read a, Read b
         ) => Applicable (a -> b -> r) where
  f @@ v | [ arg1, arg2 ] <- symToStr <$> V.toList v =
    f (read arg1)
      (read arg2)

instance ( Ground r ~ 'True
         , Read a, Read b, Read c
         ) => Applicable (a -> b -> c -> r) where
  f @@ v | [ arg1, arg2, arg3 ] <- symToStr <$> V.toList v =
    f (read arg1)
      (read arg2)
      (read arg3)

instance ( Ground r ~ 'True
         , Read a, Read b, Read c, Read d
         ) => Applicable (a -> b -> c -> d -> r) where
  f @@ v | [ arg1, arg2, arg3, arg4 ] <- symToStr <$> V.toList v =
    f (read arg1)
      (read arg2)
      (read arg3)
      (read arg4)

instance ( Ground r ~ 'True
         , Read a, Read b, Read c, Read d, Read e
         ) => Applicable (a -> b -> c -> d -> e -> r) where
  f @@ v | [ arg1, arg2, arg3, arg4, arg5 ] <- symToStr <$> V.toList v =
    f (read arg1)
      (read arg2)
      (read arg3)
      (read arg4)
      (read arg5)

toSym :: Show a => a -> Sym
toSym = Sym . pack . show

symToStr :: Sym -> [ Char ]
symToStr (Sym text) = unpack text

fromTerm :: Term -> Sym
fromTerm = \case
  TSym s -> s
  TVar{} -> panic
    "Mode error: Foreign function argument is not sufficiently bound."
