{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Exalog.Core
  ( module Language.Exalog.Annotation
  -- * Core data types
  -- ** Predicate
  , Predicate(..)
  , PredicateSym
  , Nature(..)
  , ForeignFunc
  -- ** Literal
  , Literal(..)
  , Term(..)
  , Var(..), Sym(..)
  , Polarity(..)
  -- ** Clause
  , Clause(..)
  , Head, Body
  -- ** Whole program
  , Program(..)
  -- * Existentially boxing data types
  , PredicateBox(..)
  , predicateBox
  -- * Helper type classes
  , DecorableAST(..)
  , PeelableAST(..)
  , SpannableAST
  , Formula(..)
  -- * Helper functions
  , sameArity
  , ($$)
  , search
  , findIntentionals
  ) where

import Protolude hiding (head)

import           Data.Kind (Type)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Singletons (fromSing)
import           Data.Singletons.TypeLits (SNat)
import           Data.Singletons.Prelude (sCompare)
import           Data.Singletons.Decide (Decision(..), (%~))
import qualified Data.Vector.Sized as V

import qualified GHC.Show as Show

import           Language.Exalog.Annotation
import           Language.Exalog.SrcLoc

type ForeignFunc n = V.Vector n Term -> IO (Either Text [ V.Vector n Sym ])

-- |Type indicating the nature of Datalog predicate
data Nature (n :: Nat) =
    Logical
  | Extralogical (ForeignFunc n)

type PredicateSym = Text

-- |A predicate is a predicate symbol and an arity
data Predicate (n :: Nat) a = Predicate
  { annotation :: PredicateAnn a
  , fxSym      :: PredicateSym
  , arity      :: SNat n
  , nature     :: Nature n
  }

-- |Polarity indicates whether a literal has negation in front of it
data Polarity = Positive | Negative deriving (Eq, Ord, Show)

newtype Var = Var Text deriving (Eq, Ord, Show)
data Sym =
    SymText   Text
  | SymInt    Int
  | SymDouble Double
  | SymBool   Bool
  deriving (Eq, Ord, Show)

-- |A term is a variable or a symbol
data Term = TVar Var | TSym Sym deriving (Eq, Ord, Show)

-- |If p is a predicate with arity n and (x_1,...,x_n) is a tuple of terms,
-- p(x_1,...,x_n) and neg p(x_1,...,x_n) are literals.
data Literal a = forall n . Literal
  { annotation :: LiteralAnn a
  , polarity   :: Polarity
  , predicate  :: Predicate n a
  , terms      :: V.Vector n Term
  }

type Head a = Literal a
type Body a = NE.NonEmpty (Literal a)

-- |A clause of the form p(...) :- q_1(...),...,q_k(...)
data Clause a = Clause
  { annotation :: ClauseAnn a
  , head       :: Head a
  , body       :: Body a
  }

-- |A set of clauses
data Program a = Program
  { annotation :: ProgramAnn a
  , clauses    :: [ Clause a ]
  , queryPreds :: [ PredicateBox a ]
  }

-- Map data types to correct type families for annotations
type instance Ann Program = ProgramAnn
type instance Ann Clause  = ClauseAnn
type instance Ann Literal = LiteralAnn
type instance Ann (Predicate n) = PredicateAnn

-- Helpers for stripping annotations from a tree
type family Peeled (ast :: Type) = (ast' :: Type) where
  Peeled (Program (ann a))      = Program a
  Peeled (Clause (ann a))       = Clause a
  Peeled (Literal (ann a))      = Literal a
  Peeled (Predicate n (ann a))  = Predicate n a
  Peeled (PredicateBox (ann a)) = PredicateBox a

class PeelableAST (ast :: Type) where
  peel :: ast -> Peeled ast

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Program) ann
         , PeelableAST (Clause (ann a))
         , PeelableAST (PredicateBox (ann a))
         ) => PeelableAST (Program (ann a)) where
  peel Program{..} =
    Program (peelA annotation) (fmap peel clauses) (map peel queryPreds)

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Clause) ann
         , PeelableAST (Literal (ann a))
         ) => PeelableAST (Clause (ann a)) where
  peel Clause{..} = Clause (peelA annotation) (peel head) (fmap peel body)

instance {-# OVERLAPPABLE #-}
            PeelableAnn PredicateAnn ann
         => PeelableAST (Predicate n (ann a)) where
  peel Predicate{..} = Predicate (peelA annotation) fxSym arity nature

-- Helpers for decorating the tree with an annotation
type family Decored (ast :: Type) (ann :: AnnType -> AnnType) = (ast' :: Type) | ast' -> ast ann where
  Decored (Program ann) f = Program (f ann)
  Decored (Clause ann) f = Clause (f ann)
  Decored (Literal ann) f = Literal (f ann)
  Decored (Predicate n ann) f = Predicate n (f ann)
  Decored (PredicateBox ann) f = PredicateBox (f ann)

class DecorableAST (ast :: Type) (ann :: AnnType -> AnnType) where
  decorate :: ast -> Decored ast ann

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn (Ann Program) ann
         , DecorableAST (Clause a) ann
         , DecorableAST (PredicateBox a) ann
         ) => DecorableAST (Program a) ann  where
  decorate Program{..} =
    Program (decorA annotation) (fmap decorate clauses) (map decorate queryPreds)

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn (Ann Clause) ann
         , DecorableAST (Literal a) ann
         ) => DecorableAST (Clause a) ann where
  decorate Clause{..} =
    Clause (decorA annotation) (decorate head) (fmap decorate body)

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn PredicateAnn ann
         ) => DecorableAST (Predicate n a) ann where
  decorate Predicate{..} =
    Predicate (decorA annotation) fxSym arity nature

-- Helpers relating to logical formulae
class Formula ast where
  type Annotation ast :: AnnType
  variables  :: ast -> [ Var ]
  predicates :: ast -> [ PredicateBox (Annotation ast)]

instance Formula (Literal a) where
  type Annotation (Literal a) = a

  variables Literal{terms = terms} = flip mapMaybe (V.toList terms) $ \case
    TVar var -> Just var
    TSym _   -> Nothing

  predicates Literal{predicate = predicate} = [ PredicateBox predicate ]

instance Eq (PredicateAnn a) => Formula (Clause a) where
  type Annotation (Clause a) = a

  variables Clause{..} =
    variables head ++ concatMap variables body

  predicates Clause{head = head, body = body} =
    nub $ concatMap predicates $ head : NE.toList body

instance Eq (PredicateAnn a) => Formula (Program a) where
  type Annotation (Program a) = a

  variables = panic "Obtaining variables of a program is meaningless."

  predicates (Program _ clss _) = nub $ concatMap predicates clss

-- Instances for standard type classes like Eq, Ord, Show

-- Predicate
instance Eq (PredicateAnn ann) => Eq (Predicate n ann) where
  p@Predicate{annotation = ann} == p'@Predicate{annotation = ann'} =
    fxSym p == fxSym p' && ann == ann'

instance Ord (PredicateAnn ann) => Ord (Predicate n ann) where
  p@Predicate{annotation = ann} `compare` p'@Predicate{annotation = ann'} =
    (ann, fxSym p) `compare` (ann', fxSym p')

instance Show (PredicateAnn ann) => Show (Predicate n ann) where
  show Predicate{..} =
    "Predicate{annotation = " <> show annotation <> ", " <>
    "fxSym = " <> show fxSym <> "," <>
    "arity = " <> show arity <> "}"

deriving instance
  ( Show (PredicateAnn a)
  ) => Show (PredicateBox a)

-- Literal
instance ( Eq (LiteralAnn a)
         , Eq (PredicateAnn a)
         ) => Eq (Literal a) where
  l@Literal{annotation = ann, predicate = p, terms = ts} ==
    l'@Literal{annotation = ann', predicate = p', terms = ts'}
    | Proved Refl <- sameArity p p' =
      ann == ann' &&
      p == p' &&
      polarity l == polarity l' &&
      ts == ts'
    | otherwise = False

instance ( Ord (LiteralAnn a)
         , Ord (PredicateAnn a)
         ) => Ord (Literal a) where
  Literal ann pol p@Predicate{arity = n} ts `compare`
    Literal ann' pol' p'@Predicate{arity = n'} ts'
    | Proved Refl <- sameArity p p' =
        (ann, pol, p, ts) `compare` (ann', pol', p', ts')
    | otherwise = fromSing $ sCompare n n'

deriving instance
  ( Show (LiteralAnn a)
  , Show (PredicateAnn a)
  ) => Show (Literal a)

-- Instances for obtaining sapns of AST nodes
instance {-# OVERLAPPING #-}
    SpannableAnn (ProgramAnn ann) => Spannable (Program ann) where
  span Program{..} = annSpan annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (ClauseAnn ann) => Spannable (Clause ann) where
  span Clause{..} = annSpan annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (LiteralAnn ann) => Spannable (Literal ann) where
  span Literal{..} = annSpan annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (PredicateAnn ann) => Spannable (Predicate n ann) where
  span Predicate{..} = annSpan annotation

type SpannableAST ann =
  ( Spannable (Program ann)
  , Spannable (Clause  ann)
  , Spannable (Literal ann)
  , SpannableAnn (ProgramAnn ann)
  , SpannableAnn (ClauseAnn  ann)
  , SpannableAnn (LiteralAnn ann)
  )

-- Clause
deriving instance (Ord (ClauseAnn a), Ord (Literal a)) => Ord (Clause a)
deriving instance (Eq (ClauseAnn a), Eq (Literal a)) => Eq (Clause a)
deriving instance (Show (ClauseAnn a), Show (Literal a)) => Show (Clause a)

-- Program
instance (Ord (Clause a), Eq (ProgramAnn a)) => Eq (Program a) where
  Program{annotation = ann, clauses = clss} ==
    Program{annotation = ann', clauses = clss'} =
    ann == ann' &&
    S.fromList clss == S.fromList clss'

deriving instance
  ( Show (Clause a)
  , Show (PredicateBox a)
  , Show (ProgramAnn a)
  ) => Show (Program a)

-- Relating to existentially boxing types wtih Nat
data PredicateBox a = forall n. PredicateBox (Predicate n a)

predicateBox :: Literal a -> PredicateBox a
predicateBox Literal{predicate = p} = PredicateBox p

infixr 0 $$
($$) :: (forall n. Predicate n a -> b) -> PredicateBox a -> b
f $$ (PredicateBox p) = f p

instance Eq (PredicateAnn ann) => Eq (PredicateBox ann) where
  PredicateBox p == PredicateBox p'
    | Proved Refl <- sameArity p p' = p == p'
    | otherwise = False

-- Misc. helpers

-- | Decide if two predicates have the same arity
sameArity :: Predicate n ann -> Predicate m ann -> Decision (n :~: m)
sameArity p p' = arity p %~ arity p'

-- | Find the intentional predicates of a program
findIntentionals :: Program a -> [ PredicateBox a ]
findIntentionals Program{clauses = clauses} =
  flip map (map head clauses) $ \case
    Literal{predicate = p} -> PredicateBox p

-- | Search for clauses that has the given head predicate
search :: Eq (PredicateAnn a) => Program a -> PredicateBox a -> [ Clause a ]
search pr predBox =
  [ cl | cl@Clause{head = Literal{predicate = p}} <- clauses pr
       , PredicateBox p == predBox ]
