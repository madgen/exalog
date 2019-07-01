{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Delta
  ( Decor(..)
  , updateDecor
  , decor
  , elimDecor
  , mkDeltaPredicate
  , mkDeltaLiteral
  , mkDeltaStratum
  , mkDeltaSolution
  , cleanDeltaSolution
  ) where

import Protolude hiding (head)

import           Control.Comonad (Comonad(..))

import           Language.Exalog.Pretty.Helper (Pretty(..))
import           Language.Exalog.Core
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Util.List.Zipper as LZ

data Decor = Normal | Delta | Prev | PrevX2 deriving (Eq, Ord, Show)

instance Pretty Decor where
  pretty Normal = "Norm"
  pretty Delta  = "Δ"
  pretty Prev   = "-1"
  pretty PrevX2 = "-2"

instance Pretty b => Pretty (Decor, b) where
  pretty (dec, b) = pretty dec <> "_" <> pretty b

data    instance PredicateAnn ('ADelta a) = PredADelta Decor (PredicateAnn a)
data    instance LiteralAnn ('ADelta a)   = LitADelta (LiteralAnn a)
newtype instance ClauseAnn  ('ADelta a)   = ClADelta (ClauseAnn a)
newtype instance ProgramAnn ('ADelta a)   = ProgADelta (ProgramAnn a)

deriving instance Show (PredicateAnn a) => Show (PredicateAnn ('ADelta a))
deriving instance Show (LiteralAnn a)   => Show (LiteralAnn ('ADelta a))
deriving instance Show (ClauseAnn a)    => Show (ClauseAnn ('ADelta a))
deriving instance Show (ProgramAnn a)   => Show (ProgramAnn ('ADelta a))

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('ADelta a))
deriving instance Eq (LiteralAnn a)   => Eq (LiteralAnn ('ADelta a))
deriving instance Eq (ClauseAnn a)    => Eq (ClauseAnn ('ADelta a))
deriving instance Eq (ProgramAnn a)   => Eq (ProgramAnn ('ADelta a))

deriving instance Ord (PredicateAnn a) => Ord (PredicateAnn ('ADelta a))
deriving instance Ord (LiteralAnn a)   => Ord (LiteralAnn ('ADelta a))
deriving instance Ord (ClauseAnn a)    => Ord (ClauseAnn ('ADelta a))
deriving instance Ord (ProgramAnn a)   => Ord (ProgramAnn ('ADelta a))

instance SpannableAnn (PredicateAnn a) => SpannableAnn (PredicateAnn ('ADelta a)) where
  annSpan (PredADelta _ ann) = annSpan ann
instance SpannableAnn (LiteralAnn a) => SpannableAnn (LiteralAnn ('ADelta a)) where
  annSpan (LitADelta ann) = annSpan ann
instance SpannableAnn (ClauseAnn a) => SpannableAnn (ClauseAnn ('ADelta a)) where
  annSpan (ClADelta ann) = annSpan ann
instance SpannableAnn (ProgramAnn a) => SpannableAnn (ProgramAnn ('ADelta a)) where
  annSpan (ProgADelta ann) = annSpan ann

instance IdentifiableAnn (PredicateAnn ann) b
    => IdentifiableAnn (PredicateAnn ('ADelta ann)) (Decor,b) where
  idFragment (PredADelta dec rest) = (dec, idFragment rest)
instance IdentifiableAnn (LiteralAnn ann) b
    => IdentifiableAnn (LiteralAnn ('ADelta ann)) b where
  idFragment (LitADelta rest) = idFragment rest
instance IdentifiableAnn (ClauseAnn ann) b
    => IdentifiableAnn (ClauseAnn ('ADelta ann)) b where
  idFragment (ClADelta rest) = idFragment rest
instance IdentifiableAnn (ProgramAnn ann) b
    => IdentifiableAnn (ProgramAnn ('ADelta ann)) b where
  idFragment (ProgADelta rest) = idFragment rest

updateDecor :: Decor -> Predicate n ('ADelta a) -> Predicate n ('ADelta a)
updateDecor dec p@Predicate{annotation = PredADelta _ prevAnn} =
  p {annotation = PredADelta dec prevAnn}

elimDecor :: Decor -> R.Solution ('ADelta a) -> R.Solution ('ADelta a)
elimDecor d sol = (`R.filter` sol) $ \(R.Relation p _) -> decor p /= d

decor :: Predicate n ('ADelta a) -> Decor
decor Predicate{annotation = PredADelta dec _} = dec

instance DecorableAnn LiteralAnn 'ADelta where decorA = LitADelta
instance DecorableAnn ClauseAnn  'ADelta where decorA = ClADelta
instance DecorableAnn ProgramAnn 'ADelta where decorA = ProgADelta

instance PeelableAnn PredicateAnn 'ADelta where
  peelA (PredADelta _ prevAnn) = prevAnn

-- |For each clause, generate a version for each IDB predicate where the
-- IDB predicate appears in delta form i.e. we focus on the newly generated
-- facts for the predicate in focus.
--
-- The IDB predicates that precede the delta predicate refer to the
-- previous generation and those that follow refer to the generation
-- before. This optimises repeated predicates.
--
-- It eliminates all clauses that does not have any intensional predicates
-- in its body.
mkDeltaStratum :: forall a b. Eq (PredicateBox a)
               => IdentifiableAnn (PredicateAnn a) b => Ord b
               => [ Clause a ] -> [ Clause ('ADelta a) ]
mkDeltaStratum stratum = concatMap mkCls stratum
  where
  intentionalPreds = intentionals stratum

  mkCls :: Clause a -> [ Clause ('ADelta a) ]
  mkCls Clause{..} =
      fmap (Clause (decorA annotation) (mkDeltaLiteral Delta head) . LZ.toNonEmptyList)
    . mapMaybe processBody
    . LZ.toList
    . duplicate
    . LZ.fromNonEmptyList $ body

  processBody :: LZ.Zipper (Literal a)
              -> Maybe (LZ.Zipper (Literal ('ADelta a)))
  processBody lits
    | (`elem` intentionalPreds) . predicateBox . LZ.focus $ lits = Just
      . LZ.threeWayMap (mkPrev Prev) (mkDeltaLiteral Delta) (mkPrev PrevX2) $ lits
    | otherwise = Nothing

  mkPrev :: Decor -> Literal a -> Literal ('ADelta a)
  mkPrev deco lit
    | predicateBox lit `elem` intentionalPreds = mkDeltaLiteral deco lit
    | otherwise = mkDeltaLiteral Normal lit

mkDeltaLiteral :: Decor -> Literal a -> Literal ('ADelta a)
mkDeltaLiteral deco Literal{..} = Literal
  { annotation = decorA annotation
  , predicate  = mkDeltaPredicate deco predicate
  , ..}

mkDeltaPredicate :: Decor -> Predicate n a -> Predicate n ('ADelta a)
mkDeltaPredicate deco Predicate{..} = Predicate
  { annotation = PredADelta deco annotation
  , ..}

mkDeltaSolution :: Identifiable (PredicateAnn a) b
                => [ PredicateBox a ] -> R.Solution a -> R.Solution ('ADelta a)
mkDeltaSolution intentionalPreds sol =
  intDeltas `R.merge` intPrevs `R.merge` extNormals
  where
  (intentionalSol, extensionalSol) =
    R.partition (\(R.Relation p _) -> PredicateBox p `elem` intentionalPreds) sol

  intDeltas  = R.rename (mkDeltaPredicate Delta ) intentionalSol
  intPrevs   = R.rename (mkDeltaPredicate Prev  ) intentionalSol
  extNormals = R.rename (mkDeltaPredicate Normal) extensionalSol

cleanDeltaSolution :: R.Solution ('ADelta a) -> R.Solution a
cleanDeltaSolution =
  R.rename peel . R.filter (\(R.Relation p _) -> decor p == Normal)
