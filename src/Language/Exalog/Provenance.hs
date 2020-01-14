{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Exalog.Provenance
  ( Provenance(..)
  , PredicateAnn(..)
  , LiteralAnn(..)
  , ClauseAnn(..)
  , ProgramAnn(..)
  , KnowledgeAnn(..)
  , DecorableAST
  , DecorableAnn) where

import Protolude hiding (head, pred)

import           Data.Aeson (ToJSON(..), Value(..))
import qualified Data.HashMap.Strict as HM

import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import           Language.Exalog.Pretty.Helper (Pretty(..))
import           Language.Exalog.Pretty ()

data Provenance a = Derived (Clause a) | Given deriving Generic

deriving instance Eq   (Clause a) => Eq (Provenance a)
deriving instance Ord  (Clause a) => Ord (Provenance a)
deriving instance Show (Clause a) => Show (Provenance a)

instance (Pretty (Clause a)) => Pretty (Provenance a) where
  pretty Given        = "G"
  pretty (Derived cl) = "D:" <> pretty cl

instance ToJSON (Provenance a) where

instance
  ( Pretty (Provenance a)
  , Pretty b
  ) => Pretty (Provenance a, b) where
  pretty (prov, b) = pretty prov <> "_" <> pretty b

newtype instance PredicateAnn ('AProvenance a) = PredAProvenance (PredicateAnn a)
newtype instance LiteralAnn   ('AProvenance a) = LitAProvenance (LiteralAnn a)
newtype instance ClauseAnn    ('AProvenance a) = ClAProvenance (ClauseAnn a)
newtype instance ProgramAnn   ('AProvenance a) = ProgAProvenance (ProgramAnn a)
data    instance KnowledgeAnn ('AProvenance a) = KnowAProvenance
  { _provenance :: Provenance ('AProvenance a)
  , _prevAnn    :: KnowledgeAnn a
  }

instance
  ( Identifiable (PredicateAnn a) id
  , Identifiable (Ann Literal a) id2
  , Pretty (KnowledgeAnn a)
  ) => Pretty (KnowledgeAnn ('AProvenance a)) where
  pretty (KnowAProvenance prov prev) = pretty prov <> pretty prev

instance KB.KnowledgeMaker ann => KB.KnowledgeMaker ('AProvenance ann) where
  mkKnowledge clause pred syms = KB.Knowledge
    (KnowAProvenance
        (Derived clause)
        (KB._annotation previousKnowledge)
    )
    pred
    syms
    where
    previousKnowledge =
        KB.mkKnowledge
            (peel clause)
            (peel pred)
            syms

deriving instance Show (PredicateAnn a) => Show (PredicateAnn ('AProvenance a))
deriving instance Show (LiteralAnn a)   => Show (LiteralAnn ('AProvenance a))
deriving instance Show (ClauseAnn a)    => Show (ClauseAnn ('AProvenance a))
deriving instance Show (ProgramAnn a)   => Show (ProgramAnn ('AProvenance a))
deriving instance (Show (Clause ('AProvenance a)), Show (KnowledgeAnn a)) => Show (KnowledgeAnn ('AProvenance a))

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('AProvenance a))
deriving instance Eq (LiteralAnn a)   => Eq (LiteralAnn ('AProvenance a))
deriving instance Eq (ClauseAnn a)    => Eq (ClauseAnn ('AProvenance a))
deriving instance Eq (ProgramAnn a)   => Eq (ProgramAnn ('AProvenance a))
deriving instance (
    IdentifiableAnn (ClauseAnn a) id1,
    IdentifiableAnn (PredicateAnn a) id2,
    IdentifiableAnn (LiteralAnn a) id3,
    Eq id1, Eq id2, Eq id3,
    Eq (KnowledgeAnn a)
    ) => Eq (KnowledgeAnn ('AProvenance a))

deriving instance Ord (PredicateAnn a) => Ord (PredicateAnn ('AProvenance a))
deriving instance Ord (LiteralAnn a)   => Ord (LiteralAnn ('AProvenance a))
deriving instance Ord (ClauseAnn a)    => Ord (ClauseAnn ('AProvenance a))
deriving instance Ord (ProgramAnn a)   => Ord (ProgramAnn ('AProvenance a))
deriving instance (
    IdentifiableAnn (ClauseAnn a) id1,
    IdentifiableAnn (PredicateAnn a) id2,
    IdentifiableAnn (LiteralAnn a) id3,
    Ord id1, Ord id2, Ord id3,
    Ord (KnowledgeAnn a)
    ) => Ord (KnowledgeAnn ('AProvenance a))

instance SpannableAnn (PredicateAnn a) => SpannableAnn (PredicateAnn ('AProvenance a)) where
  annSpan (PredAProvenance ann) = annSpan ann
instance SpannableAnn (LiteralAnn a) => SpannableAnn (LiteralAnn ('AProvenance a)) where
  annSpan (LitAProvenance ann) = annSpan ann
instance SpannableAnn (ClauseAnn a) => SpannableAnn (ClauseAnn ('AProvenance a)) where
  annSpan (ClAProvenance ann) = annSpan ann
instance SpannableAnn (ProgramAnn a) => SpannableAnn (ProgramAnn ('AProvenance a)) where
  annSpan (ProgAProvenance ann) = annSpan ann

instance IdentifiableAnn (PredicateAnn ann) b
    => IdentifiableAnn (PredicateAnn ('AProvenance ann)) b where
  idFragment (PredAProvenance rest) = idFragment rest
instance IdentifiableAnn (LiteralAnn ann) b
    => IdentifiableAnn (LiteralAnn ('AProvenance ann)) b where
  idFragment (LitAProvenance rest) = idFragment rest
instance IdentifiableAnn (ClauseAnn ann) b
    => IdentifiableAnn (ClauseAnn ('AProvenance ann)) b where
  idFragment (ClAProvenance rest) = idFragment rest
instance IdentifiableAnn (ProgramAnn ann) b
    => IdentifiableAnn (ProgramAnn ('AProvenance ann)) b where
  idFragment (ProgAProvenance rest) = idFragment rest
instance IdentifiableAnn (KnowledgeAnn ann) b
    => IdentifiableAnn (KnowledgeAnn ('AProvenance ann)) (Provenance ('AProvenance ann), b) where
  idFragment (KnowAProvenance provenance rest) = (provenance, idFragment rest)

instance PeelableAnn PredicateAnn 'AProvenance where
  peelA (PredAProvenance prevAnn) = prevAnn
instance PeelableAnn ClauseAnn 'AProvenance where
  peelA (ClAProvenance prevAnn) = prevAnn
instance PeelableAnn LiteralAnn 'AProvenance where
  peelA (LitAProvenance prevAnn) = prevAnn
instance PeelableAnn KnowledgeAnn 'AProvenance where
  peelA (KnowAProvenance _ prevAnn) = prevAnn

instance PeelableAST (Literal ('AProvenance a)) where
  peel Literal{..} = Literal
    { _annotation = peelA _annotation
    , _predicate  = peel  _predicate
    , ..}

instance PeelableAST (KB.Knowledge ('AProvenance a)) where
  peel KB.Knowledge{..} = KB.Knowledge
    { _annotation = peelA _annotation
    , _predicate  = peel _predicate
    , ..}

instance DecorableAnn PredicateAnn 'AProvenance where
    decorA = PredAProvenance
instance DecorableAnn LiteralAnn 'AProvenance where
    decorA = LitAProvenance
instance DecorableAnn ClauseAnn 'AProvenance where
    decorA = ClAProvenance
instance DecorableAnn KnowledgeAnn 'AProvenance where
    decorA = KnowAProvenance Given
instance DecorableAnn ProgramAnn 'AProvenance where
    decorA = ProgAProvenance

instance DecorableAST (Literal a) 'AProvenance where
    decorate Literal{..} =
        Literal { _annotation = decorA   _annotation
                , _predicate  = decorate _predicate
                , ..}

instance DecorableAST (KB.Knowledge a) 'AProvenance where
    decorate KB.Knowledge{..} =
        KB.Knowledge { _annotation = decorA _annotation
                     , _predicate  = decorate _predicate
                     , ..}

instance ToJSON (KB.Knowledge ('AProvenance 'ABase)) where
  toJSON kb@KB.Knowledge{..} =
    case toJSON $ peel kb of
      Object o -> Object $ HM.insert "provenance" provenance o
      _ -> panic "Knowledge does not produce a JSON object."
    where
    provenance = toJSON $ _provenance _annotation
