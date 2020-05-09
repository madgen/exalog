{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Exalog.Core
  ( module Language.Exalog.Annotation
  -- * Core data types
  -- ** Predicate
  , Predicate(..)
  , PredicateSymbol(..)
  , Nature(..)
  , Foreign
  , ForeignFunc
  -- ** Literal
  , Literal(..)
  , Term(..)
  , Var(..), Sym(..)
  , Polarity(..)
  -- ** Clause
  , Clause(..)
  , Head, Body
  -- ** Stratum
  , Stratum(..)
  -- ** Whole program
  , Program(..)
  -- * Existentially boxing data types
  , PredicateBox(..)
  , predicateBox
  , SomeForeignFunc(..)
  , SomeNature(..)
  -- * Helper type classes
  , DecorableAST(..)
  , PeelableAST(..)
  , SpannableAST
  , Formula(..)
  , HasIntentionals(..)
  , Decored
  , Peeled
  -- * Helper functions
  , literals
  , sameArity
  , ($$)
  , search
  , stratumOver
  , stratumOverF
  , stratumOverA_
  ) where

import Protolude hiding (head, pred, bool)

import GHC.Base (coerce)

import           Data.Aeson (Value(..), ToJSON(..), (.=), object)
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Data.Singletons (fromSing)
import           Data.Singletons.TypeLits (SNat)
import           Data.Singletons.Prelude (sCompare)
import           Data.Singletons.Decide (Decision(..), (%~))
import qualified Data.Vector.Sized as V

import qualified GHC.Show as Show

import           Language.Exalog.Annotation
import           Language.Exalog.SrcLoc

type Foreign a = ExceptT Text IO a
type ForeignFunc n = V.Vector n Term -> Foreign [ V.Vector n Sym ]
data SomeForeignFunc = forall n. KnownNat n => SFF (ForeignFunc n)

-- |Type indicating the nature of Datalog predicate
data Nature (n :: Nat) =
    Logical
  | Extralogical (ForeignFunc n)
data SomeNature = forall n. KnownNat n => SN (Nature n)

newtype PredicateSymbol = PredicateSymbol Text
  deriving (Eq, Ord, Show, IsString)

-- |A predicate is a predicate symbol and an arity
data Predicate (n :: Nat) a = Predicate
  { _annotation :: PredicateAnn a
  , _predSym    :: PredicateSymbol
  , _arity      :: SNat n
  , _nature     :: Nature n
  }

-- |Polarity indicates whether a literal has negation in front of it
data Polarity = Positive | Negative deriving (Eq, Ord, Show, Generic)

newtype Var = Var Text deriving (Eq, Ord, Show)
data Sym =
    SymText   Text
  | SymInt    Int
  | SymBool   Bool
  deriving (Eq, Ord, Show)

-- |A term is a variable or a symbol
data Term = TVar Var | TSym Sym | TWild deriving (Eq, Ord, Show, Generic)

-- |If p is a predicate with arity n and (x_1,...,x_n) is a tuple of terms,
-- p(x_1,...,x_n) and neg p(x_1,...,x_n) are literals.
data Literal a = forall n . Literal
  { _annotation :: LiteralAnn a
  , _polarity   :: Polarity
  , _predicate  :: Predicate n a
  , _terms      :: V.Vector n Term
  }

type Head a = Literal a
type Body a = NE.NonEmpty (Literal a)

-- |A clause of the form p(...) :- q_1(...),...,q_k(...)
data Clause a = Clause
  { _annotation :: ClauseAnn a
  , _head       :: Head a
  , _body       :: Body a
  }

stratumOverF :: Functor f => ([ Clause a ] -> f [ Clause b ]) -> Stratum a -> f (Stratum b)
stratumOverF f stratum = Stratum <$> f (coerce stratum)

stratumOverA_ :: Applicative f => ([ Clause a ] -> f ()) -> Stratum a -> f ()
stratumOverA_ f = f . coerce

stratumOver :: ([ Clause a ] -> [ Clause b ]) -> Stratum a -> Stratum b
stratumOver = coerce

newtype Stratum a = Stratum { _unStratum :: [ Clause a ] }

-- |A set of clauses
data Program a = Program
  { _annotation :: ProgramAnn a
  , _strata     :: [ Stratum a ]
  , _queries    :: [ PredicateBox a ]
  }

-- Map data types to correct type families for annotations
type instance Ann Program = ProgramAnn
type instance Ann Clause  = ClauseAnn
type instance Ann Literal = LiteralAnn
type instance Ann (Predicate n) = PredicateAnn

-- Helpers for stripping annotations from a tree
type family Peeled (ast :: Type) = (ast' :: Type)
type instance Peeled (Program (ann a))      = Program a
type instance Peeled (Clause (ann a))       = Clause a
type instance Peeled (Literal (ann a))      = Literal a
type instance Peeled (Predicate n (ann a))  = Predicate n a
type instance Peeled (PredicateBox (ann a)) = PredicateBox a

class PeelableAST (ast :: Type) where
  peel :: ast -> Peeled ast

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Program) ann
         , PeelableAST (Clause (ann a))
         , PeelableAST (PredicateBox (ann a))
         ) => PeelableAST (Program (ann a)) where
  peel Program{..} =
    Program (peelA _annotation) (stratumOver (map peel) <$> _strata) (map peel _queries)

instance {-# OVERLAPPABLE #-}
         ( PeelableAnn (Ann Clause) ann
         , PeelableAST (Literal (ann a))
         ) => PeelableAST (Clause (ann a)) where
  peel Clause{..} = Clause (peelA _annotation) (peel _head) (fmap peel _body)

instance {-# OVERLAPPABLE #-}
            PeelableAnn PredicateAnn ann
         => PeelableAST (Predicate n (ann a)) where
  peel Predicate{..} = Predicate (peelA _annotation) _predSym _arity _nature

instance {-# OVERLAPPABLE #-}
            PeelableAnn PredicateAnn ann
         => PeelableAST (PredicateBox (ann a)) where
  peel (PredicateBox p) = PredicateBox $ peel p

-- Helpers for decorating the tree with an annotation
type family Decored (ast :: Type) (ann :: AnnType -> AnnType) = (ast' :: Type) | ast' -> ast ann
type instance Decored (Program ann) f = Program (f ann)
type instance Decored (Clause ann) f = Clause (f ann)
type instance Decored (Literal ann) f = Literal (f ann)
type instance Decored (Predicate n ann) f = Predicate n (f ann)
type instance Decored (PredicateBox ann) f = PredicateBox (f ann)

class DecorableAST (ast :: Type) (ann :: AnnType -> AnnType) where
  decorate :: ast -> Decored ast ann

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn (Ann Program) ann
         , DecorableAST (Clause a) ann
         , DecorableAST (PredicateBox a) ann
         ) => DecorableAST (Program a) ann  where
  decorate Program{..} =
    Program (decorA _annotation)
            (stratumOver (decorate <$>) <$> _strata)
            (map decorate _queries)

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn (Ann Clause) ann
         , DecorableAST (Literal a) ann
         ) => DecorableAST (Clause a) ann where
  decorate Clause{..} =
    Clause (decorA _annotation) (decorate _head) (fmap decorate _body)

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn PredicateAnn ann
         ) => DecorableAST (PredicateBox a) ann where
  decorate (PredicateBox p) = PredicateBox (decorate p)

instance {-# OVERLAPPABLE #-}
         ( DecorableAnn PredicateAnn ann
         ) => DecorableAST (Predicate n a) ann where
  decorate Predicate{..} =
    Predicate (decorA _annotation) _predSym _arity _nature

-- Helpers relating to logical formulae
class Formula ast where
  type Annotation ast :: AnnType
  variables  :: ast -> [ Var ]
  predicates :: ast -> [ PredicateBox (Annotation ast)]

instance Formula (Literal a) where
  type Annotation (Literal a) = a

  variables Literal{..} = flip mapMaybe (V.toList _terms) $ \case
    TVar var -> Just var
    TSym _   -> Nothing
    TWild    -> Nothing

  predicates Literal{..} = [ PredicateBox _predicate ]

instance ( IdentifiableAnn (PredicateAnn a) b
         , Ord b
         ) => Formula (Clause a) where
  type Annotation (Clause a) = a

  variables Clause{..} =
    variables _head ++ concatMap variables _body

  predicates Clause{..} =
    nub $ concatMap predicates $ _head : NE.toList _body

instance ( IdentifiableAnn (PredicateAnn a) b
         , Ord b
         ) => Formula (Program a) where
  type Annotation (Program a) = a

  variables = panic "Obtaining variables of a program is meaningless."

  predicates (Program _ strata _) = nub $ concatMap predicates (join $ map _unStratum strata)

-- Instances for standard type classes like Eq, Ord, Show

-- Terms
instance ToJSON Var where toJSON (Var v) = toJSON v
instance ToJSON Sym where
  toJSON (SymText text)   = String text
  toJSON (SymBool bool)   = Bool bool
  toJSON (SymInt  int)    = Number (fromIntegral int)

instance ToJSON Term where

instance ToJSON (V.Vector n Sym)  where toJSON = toJSONList . V.toList
instance ToJSON (V.Vector n Term) where toJSON = toJSONList . V.toList

-- Predicate
instance ( IdentifiableAnn (PredicateAnn ann) b
         , Eq b
         ) => Eq (Predicate n ann) where
  p@Predicate{_annotation = ann} == p'@Predicate{_annotation = ann'} =
    _predSym p == _predSym p' && idFragment ann == idFragment ann'

instance ( IdentifiableAnn (PredicateAnn ann) b
         , Ord b
         ) => Ord (Predicate n ann) where
  p@Predicate{_annotation = ann} `compare` p'@Predicate{_annotation = ann'} =
    (idFragment ann, _predSym p) `compare` (idFragment ann', _predSym p')

instance Show (PredicateAnn ann) => Show (Predicate n ann) where
  show Predicate{..} =
    "Predicate{annotation = " <> show _annotation <> ", " <>
    "fxSym = " <> show _predSym <> "," <>
    "arity = " <> show _arity <> "}"

deriving instance
  ( Show (PredicateAnn a)
  ) => Show (PredicateBox a)

instance ToJSON (Predicate n ann) where
  toJSON Predicate{_predSym = PredicateSymbol predSym} = toJSON predSym

-- Literal
instance ( IdentifiableAnn (PredicateAnn a) b
         , IdentifiableAnn (LiteralAnn a) c
         , Eq b
         , Eq c
         ) => Eq (Literal a) where
  Literal{_annotation = ann, _predicate = pred, _terms = terms, _polarity = pol} ==
    Literal{_annotation = ann', _predicate = pred', _terms = terms', _polarity = pol'}
    | Proved Refl <- sameArity pred pred' =
      idFragment ann == idFragment ann' &&
      pred == pred' &&
      pol == pol' &&
      terms == terms'
    | otherwise = False

instance ( IdentifiableAnn (PredicateAnn a) b
         , IdentifiableAnn (LiteralAnn a) c
         , Ord b
         , Ord c
         ) => Ord (Literal a) where
  Literal ann pol pred@Predicate{_arity = n} ts `compare`
    Literal ann' pol' pred'@Predicate{_arity = n'} ts'
    | Proved Refl <- sameArity pred pred' =
        (idFragment ann, pol, pred, ts) `compare` (idFragment ann', pol', pred', ts')
    | otherwise = fromSing $ sCompare n n'

deriving instance
  ( Show (LiteralAnn a)
  , Show (PredicateAnn a)
  ) => Show (Literal a)

instance ToJSON Polarity where

instance ToJSON (Literal ann) where
  toJSON Literal{..} = object
    [ "predicate" .= toJSON _predicate
    , "polarity"  .= toJSON _polarity
    , "terms"     .= toJSON _terms
    ]

-- Instances for obtaining sapns of AST nodes
instance {-# OVERLAPPING #-}
    SpannableAnn (ProgramAnn ann) => Spannable (Program ann) where
  span Program{..} = annSpan _annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (ClauseAnn ann) => Spannable (Clause ann) where
  span Clause{..} = annSpan _annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (LiteralAnn ann) => Spannable (Literal ann) where
  span Literal{..} = annSpan _annotation
instance {-# OVERLAPPING #-}
    SpannableAnn (PredicateAnn ann) => Spannable (Predicate n ann) where
  span Predicate{..} = annSpan _annotation

type SpannableAST ann =
  ( Spannable (Program ann)
  , Spannable (Clause  ann)
  , Spannable (Literal ann)
  , SpannableAnn (ProgramAnn   ann)
  , SpannableAnn (ClauseAnn    ann)
  , SpannableAnn (LiteralAnn   ann)
  , SpannableAnn (PredicateAnn ann)
  )

-- Clause
instance ( IdentifiableAnn (ClauseAnn a) b
         , Eq b
         , Eq (Literal a)
         ) => Eq (Clause a) where
  Clause ann head body == Clause ann' head' body' =
    idFragment ann == idFragment ann' &&
    head == head' &&
    body == body'
instance ( IdentifiableAnn (ClauseAnn a) b
         , Ord b
         , Ord (Literal a)
         ) => Ord (Clause a) where
  Clause ann head body `compare` Clause ann' head' body' =
    (idFragment ann, head, body) `compare` (idFragment ann', head', body')

deriving instance (Show (ClauseAnn a), Show (Literal a)) => Show (Clause a)

instance ToJSON (Clause ann) where
  toJSON Clause{..} = object
    [ "head" .= toJSON _head
    , "body" .= toJSON _body
    ]

-- Stratum
instance Ord (Clause a) => Eq (Stratum a) where
  Stratum clss == Stratum clss' = S.fromList clss == S.fromList clss'

deriving instance Ord (Clause a)  => Ord (Stratum a)
deriving instance Show (Clause a) => Show (Stratum a)

-- Program
instance ( IdentifiableAnn (ProgramAnn a) b, Eq b
         , IdentifiableAnn (PredicateAnn a) c
         , Ord (PredicateBox a)
         , Ord (Clause a)
         ) => Eq (Program a) where
  Program{_annotation = ann, _strata = strat, _queries = qPreds} ==
    Program{_annotation = ann', _strata = strat', _queries = qPreds'} =
    idFragment ann == idFragment ann' &&
    S.fromList strat == S.fromList strat' &&
    S.fromList qPreds == S.fromList qPreds'

deriving instance
  ( Show (Clause a)
  , Show (PredicateBox a)
  , Show (ProgramAnn a)
  ) => Show (Program a)

-- Relating to existentially boxing types wtih Nat
data PredicateBox a = forall n. PredicateBox (Predicate n a)

predicateBox :: Literal a -> PredicateBox a
predicateBox Literal{_predicate = pred} = PredicateBox pred

infixr 0 $$
($$) :: (forall n. Predicate n a -> b) -> PredicateBox a -> b
f $$ (PredicateBox p) = f p

instance ( IdentifiableAnn (PredicateAnn ann) b
         , Eq b
         ) => Eq (PredicateBox ann) where
  PredicateBox p == PredicateBox p'
    | Proved Refl <- sameArity p p' = p == p'
    | otherwise = False

instance ( IdentifiableAnn (PredicateAnn ann) b
         , Ord b
         ) => Ord (PredicateBox ann) where
  PredicateBox pred `compare` PredicateBox pred'
    | Proved Refl <- sameArity pred pred' = pred `compare` pred'
    | otherwise = fromSing (_arity pred) `compare` fromSing (_arity pred')

-- Misc. helpers

literals :: Clause ann -> NE.NonEmpty (Literal ann)
literals Clause{..} = _head `NE.cons` _body

-- | Decide if two predicates have the same arity
sameArity :: Predicate n ann -> Predicate m ann -> Decision (n :~: m)
sameArity p p' = _arity p %~ _arity p'

class HasIntentionals ast ann | ast -> ann where
  intentionals :: ast -> [ PredicateBox ann ]

instance (Ord (PredicateBox ann)) => HasIntentionals (Stratum ann) ann where
  intentionals stratum = nub
                       $ (\Literal{_predicate = pred} -> PredicateBox pred)
                     <$> map _head (_unStratum stratum)

instance (Ord (PredicateBox ann)) => HasIntentionals (Program ann) ann where
  -- No need to $nub$ because different strata can't have the same
  -- intentional predicates.
  intentionals Program{_strata = strata} = mconcat $ map intentionals strata

-- | Search for clauses that has the given head predicate
search :: Identifiable (PredicateAnn a) b
       => Program a -> PredicateBox a -> [ Clause a ]
search pr predBox =
  [ cl | Stratum clauses <- _strata pr
       , cl@Clause{_head = Literal{_predicate = pred}} <- clauses
       , PredicateBox pred == predBox ]
