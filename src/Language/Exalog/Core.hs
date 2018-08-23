{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Language.Exalog.Core
  ( module Language.Exalog.Annotation
  , Predicate(..), PredicateBox(..)
  , Var(..), Sym(..)
  , Term(..)
  , Unifier
  , Literal(..), Head, Body
  , Nature(..), NatureBox(..)
  , Polarity(..)
  , Clause(..)
  , Program(..)
  -- Convenience functions
  , sameArity
  , predicateBox
  , ($$)
  , search
  , findIntentionals
  -- Helper type classes
  , DecorableAST(..)
  , PeelableAST(..)
  , Formula(..)
  ) where

import Protolude hiding (Nat, head)

import           Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Singletons.Decide (Decision(..), (%~))

import           Language.Exalog.Annotation

import           Util.Vector as V

type Unifier = [ (Var, Sym) ]

-- |Type indicating the nature of Datalog predicate
data Nature (n :: Nat) =
    Logical
  | Extralogical (Vector n Term -> IO (Either Text [ Unifier ]))

data NatureBox = forall n. NatureBox (Nature n)

type PredicateSym = Text

-- |A predicate is a predicate symbol and an arity
data Predicate (n :: Nat) a = Predicate
  { annotation :: PredicateAnn a
  , fxSym      :: PredicateSym
  , arity      :: SNat n
  , nature     :: Nature n
  }

sameArity :: Predicate n ann -> Predicate m ann -> Decision (n :~: m)
sameArity p p' = arity p %~ arity p'

instance Eq (PredicateAnn ann) => Eq (Predicate n ann) where
  p@Predicate{annotation = ann} == p'@Predicate{annotation = ann'} =
    fxSym p == fxSym p' && ann == ann'

data PredicateBox a = forall n. PredicateBox (Predicate n a)

infixr 0 $$
($$) :: (forall n. Predicate n a -> b) -> PredicateBox a -> b
f $$ (PredicateBox p) = f p

instance Eq (PredicateAnn ann) => Eq (PredicateBox ann) where
  PredicateBox p == PredicateBox p'
    | Proved Refl <- sameArity p p' = p == p'
    | otherwise = False

-- |Polarity indicates whether a literal has negation in front of it
data Polarity = Positive | Negative deriving (Eq)

newtype Var = Var Text deriving (Eq)
newtype Sym = Sym Text deriving (Eq, Ord)

-- |A term is a variable or a symbol
data Term = TVar Var | TSym Sym

-- |If p is a predicate with arity n and (x_1,...,x_n) is a tuple of terms,
-- p(x_1,...,x_n) and neg p(x_1,...,x_n) are literals.
data Literal a = forall n . Literal
  { annotation :: LiteralAnn a
  , polarity   :: Polarity
  , predicate  :: Predicate n a
  , terms      :: Vector n Term
  }

predicateBox :: Literal a -> PredicateBox a
predicateBox Literal{predicate = p} = PredicateBox p

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
  }

-- |Find the intentional predicates of a program
findIntentionals :: Program a -> [ PredicateBox a ]
findIntentionals Program{clauses = clauses} =
  flip map (map head clauses) $ \case
    Literal{predicate = p} -> PredicateBox p

-- |Search for clauses that has the given head predicate
search :: Eq (PredicateAnn a) => Program a -> PredicateBox a -> [ Clause a ]
search pr predBox =
  [ cl | cl@Clause{head = Literal{predicate = p}} <- clauses pr
       , PredicateBox p == predBox ]

type instance Ann Program = ProgramAnn
type instance Ann Clause  = ClauseAnn
type instance Ann Literal = LiteralAnn
type instance Ann (Predicate n) = PredicateAnn

-- |Peeling a program
type family Peeled (ast :: Type) = (ast' :: Type) where
  Peeled (Program (ann a))     = Program a
  Peeled (Clause (ann a))      = Clause a
  Peeled (Literal (ann a))     = Literal a
  Peeled (Predicate n (ann a)) = Predicate n a

class PeelableAST (ast :: Type) where
  peel :: ast -> Peeled ast

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Program) ann
         , PeelableAST (Clause (ann a))
         ) => PeelableAST (Program (ann a)) where
  peel Program{..} = Program (peelA annotation) (fmap peel clauses)

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Clause) ann
         , PeelableAST (Literal (ann a))
         ) => PeelableAST (Clause (ann a)) where
  peel Clause{..} = Clause (peelA annotation) (peel head) (fmap peel body)

instance {-# OVERLAPPABLE #-}
            PeelableAnn PredicateAnn ann
         => PeelableAST (Predicate n (ann a)) where
  peel Predicate{..} = Predicate (peelA annotation) fxSym arity nature

-- |Decorate the AST with a new annotation layer
type family Decored (ast :: Type) (ann :: AnnType -> AnnType) = (ast' :: Type) | ast' -> ast ann where
  Decored (Program ann) f = Program (f ann)
  Decored (Clause ann) f = Clause (f ann)
  Decored (Literal ann) f = Literal (f ann)
  Decored (Predicate n ann) f = Predicate n (f ann)

class DecorableAST (ast :: Type) (ann :: AnnType -> AnnType) where
  decorate :: ast -> Decored ast ann

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn (Ann Program) ann
         , DecorableAST (Clause a) ann
         ) => DecorableAST (Program a) ann  where
  decorate Program{..} = Program (decorA annotation) (fmap decorate clauses)

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

-- |Overaloded helper methods
class Formula (ast :: AnnType -> Type) where
  variables  :: ast a -> [ Var ]
  predicates :: ast a -> [ PredicateBox a ]

instance Formula Literal where
  variables Literal{terms = terms} = flip mapMaybe (V.toList terms) $ \case
    TVar var -> Just var
    TSym _   -> Nothing

  predicates Literal{predicate = predicate} = [ PredicateBox predicate ]

instance Formula Clause where
  variables Clause{..} =
    variables head ++ concatMap variables body

  predicates Clause{head = head, body = body} =
    concatMap predicates $ head : NE.toList body

instance Formula Program where
  variables = panic "Obtaining variables of a program is meaningless."
  predicates (Program _ clss) = concatMap predicates clss
