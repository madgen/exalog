{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Protolude hiding (head, pred)

import           Control.Comonad (Comonad(..))

import           Language.Exalog.Pretty.Helper (Pretty(..))
import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.Util.List.Zipper as LZ

data Decor = Constant | Current | Delta | Prev deriving (Eq, Ord, Show)

instance Pretty Decor where
  pretty Constant = "Constant"
  pretty Current  = "Current"
  pretty Delta    = "Δ"
  pretty Prev     = "-1"

instance Pretty b => Pretty (Decor, b) where
  pretty (dec, b) = pretty dec <> "_" <> pretty b

data    instance PredicateAnn ('ADelta a) = PredADelta Decor (PredicateAnn a)
data    instance LiteralAnn ('ADelta a)   = LitADelta (LiteralAnn a)
newtype instance ClauseAnn  ('ADelta a)   = ClADelta (ClauseAnn a)
newtype instance ProgramAnn ('ADelta a)   = ProgADelta (ProgramAnn a)
newtype instance KnowledgeAnn ('ADelta a) = KnowADelta (KnowledgeAnn a)

instance KB.KnowledgeMaker ann => KB.KnowledgeMaker ('ADelta ann) where
  mkKnowledge pred syms = KB.Knowledge (KnowADelta (KB._annotation (KB.mkKnowledge (peel pred) syms))) pred syms

deriving instance Show (PredicateAnn a) => Show (PredicateAnn ('ADelta a))
deriving instance Show (LiteralAnn a)   => Show (LiteralAnn ('ADelta a))
deriving instance Show (ClauseAnn a)    => Show (ClauseAnn ('ADelta a))
deriving instance Show (ProgramAnn a)   => Show (ProgramAnn ('ADelta a))
deriving instance Show (KnowledgeAnn a) => Show (KnowledgeAnn ('ADelta a))

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('ADelta a))
deriving instance Eq (LiteralAnn a)   => Eq (LiteralAnn ('ADelta a))
deriving instance Eq (ClauseAnn a)    => Eq (ClauseAnn ('ADelta a))
deriving instance Eq (ProgramAnn a)   => Eq (ProgramAnn ('ADelta a))
deriving instance Eq (KnowledgeAnn a) => Eq (KnowledgeAnn ('ADelta a))

deriving instance Ord (PredicateAnn a) => Ord (PredicateAnn ('ADelta a))
deriving instance Ord (LiteralAnn a)   => Ord (LiteralAnn ('ADelta a))
deriving instance Ord (ClauseAnn a)    => Ord (ClauseAnn ('ADelta a))
deriving instance Ord (ProgramAnn a)   => Ord (ProgramAnn ('ADelta a))
deriving instance Ord (KnowledgeAnn a) => Ord (KnowledgeAnn ('ADelta a))

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
instance IdentifiableAnn (KnowledgeAnn ann) b
    => IdentifiableAnn (KnowledgeAnn ('ADelta ann)) b where
  idFragment (KnowADelta rest) = idFragment rest

updateDecor :: Decor -> Predicate n ('ADelta a) -> Predicate n ('ADelta a)
updateDecor dec p@Predicate{_annotation = PredADelta _ prevAnn} =
  p {_annotation = PredADelta dec prevAnn}

elimDecor :: KB.Knowledgeable kb ('ADelta a) => Decor -> kb ('ADelta a) -> kb ('ADelta a)
elimDecor d sol = (`KB.filter` sol) $ \(KB.Knowledge _ p _) -> decor p /= d

decor :: Predicate n ('ADelta a) -> Decor
decor Predicate{_annotation = PredADelta dec _} = dec

instance DecorableAnn LiteralAnn   'ADelta where decorA = LitADelta
instance DecorableAnn ClauseAnn    'ADelta where decorA = ClADelta
instance DecorableAnn ProgramAnn   'ADelta where decorA = ProgADelta
instance DecorableAnn KnowledgeAnn 'ADelta where decorA = KnowADelta

instance PeelableAnn PredicateAnn 'ADelta where
  peelA (PredADelta _ prevAnn) = prevAnn
instance PeelableAnn KnowledgeAnn 'ADelta where
  peelA (KnowADelta prevAnn) = prevAnn

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
               => Stratum a -> Stratum ('ADelta a)
mkDeltaStratum stratum@(Stratum cls) = Stratum $ concatMap mkCls cls
  where
  intentionalPreds = intentionals stratum

  mkCls :: Clause a -> [ Clause ('ADelta a) ]
  mkCls Clause{..} =
      fmap (Clause (decorA _annotation) (mkDeltaLiteral Delta _head) . LZ.toNonEmptyList)
    . mapMaybe processBody
    . LZ.toList
    . duplicate
    . LZ.fromNonEmptyList $ _body

  processBody :: LZ.Zipper (Literal a)
              -> Maybe (LZ.Zipper (Literal ('ADelta a)))
  processBody lits
    | (`elem` intentionalPreds) . predicateBox . LZ.focus $ lits = Just
      . LZ.threeWayMap (mkPrev Current) (mkDeltaLiteral Delta) (mkPrev Prev) $ lits
    | otherwise = Nothing

  mkPrev :: Decor -> Literal a -> Literal ('ADelta a)
  mkPrev deco lit
    | predicateBox lit `elem` intentionalPreds = mkDeltaLiteral deco lit
    | otherwise = mkDeltaLiteral Constant lit

mkDeltaLiteral :: Decor -> Literal a -> Literal ('ADelta a)
mkDeltaLiteral deco Literal{..} = Literal
  { _annotation = decorA _annotation
  , _predicate  = mkDeltaPredicate deco _predicate
  , ..}

mkDeltaPredicate :: Decor -> Predicate n a -> Predicate n ('ADelta a)
mkDeltaPredicate deco Predicate{..} = Predicate
  { _annotation = PredADelta deco _annotation
  , ..}

mkDeltaSolution :: Semigroup (kb ('ADelta a))
                => Identifiable (PredicateAnn a) id
                => Identifiable (KnowledgeAnn a) id1
                => KB.Knowledgeable kb a
                => [ PredicateBox a ] -> kb a -> kb ('ADelta a)
mkDeltaSolution intentionalPreds kb =
  intDeltas <> intPrevs <> extCurrents
  where
  (intentionalKB, extensionalKB) =
    KB.partition (\(KB.Knowledge _ p _) -> PredicateBox p `elem` intentionalPreds) kb

  intDeltas   = KB.atEach (mkDeltaKnowledge Delta) intentionalKB
  intPrevs    = KB.atEach (mkDeltaKnowledge Prev) intentionalKB
  extCurrents = KB.atEach (mkDeltaKnowledge Constant) extensionalKB

mkDeltaKnowledge :: Decor -> KB.Knowledge ann -> KB.Knowledge ('ADelta ann) 
mkDeltaKnowledge decoration (KB.Knowledge ann pred syms) = 
  KB.Knowledge (decorA ann) (mkDeltaPredicate decoration pred) syms

cleanDeltaSolution :: KB.Knowledgeable kb ('ADelta a)
                   => Identifiable (PredicateAnn a) id
                   => Identifiable (KnowledgeAnn a) id1
                   => kb ('ADelta a) -> kb a
cleanDeltaSolution = KB.atEach (\(KB.Knowledge ann pred syms) -> KB.Knowledge (peelA ann) (peel pred) syms)
                   . KB.filter isCurrentOrConstant
  where
  isCurrentOrConstant (KB.Knowledge _ p _) = decor p `elem` [ Current, Constant ]
