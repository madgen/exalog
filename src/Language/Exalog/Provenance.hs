{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Exalog.Provenance
  () where

import Protolude hiding (head, pred)

import           Language.Exalog.Pretty.Helper (Pretty(..))
import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Class as KB

newtype instance PredicateAnn ('AProvenance a) = PredAProvenance (PredicateAnn a)
newtype instance LiteralAnn   ('AProvenance a) = LitAProvenance (LiteralAnn a)
newtype instance ClauseAnn    ('AProvenance a) = ClAProvenance (ClauseAnn a)
newtype instance ProgramAnn   ('AProvenance a) = ProgAProvenance (ProgramAnn a)
data instance KnowledgeAnn    ('AProvenance a) = KnowAProvenance {_clause :: Clause ('AProvenance a), _prevAnn :: KnowledgeAnn a }

instance KB.KnowledgeMaker ann => KB.KnowledgeMaker ('AProvenance ann) where
  mkKnowledge clause pred syms = KB.Knowledge
    (KnowAProvenance
        clause
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
    => IdentifiableAnn (KnowledgeAnn ('AProvenance ann)) (Clause ('AProvenance ann), b) where
  idFragment (KnowAProvenance clause rest) = (clause, idFragment rest)

instance PeelableAnn PredicateAnn 'AProvenance where
  peelA (PredAProvenance prevAnn) = prevAnn
instance PeelableAnn ClauseAnn 'AProvenance where
  peelA (ClAProvenance prevAnn) = prevAnn
instance PeelableAnn LiteralAnn 'AProvenance where
  peelA (LitAProvenance prevAnn) = prevAnn
instance PeelableAnn KnowledgeAnn 'AProvenance where
  peelA (KnowAProvenance _ prevAnn) = prevAnn

instance PeelableAST (Literal ('AProvenance a)) where
    peel Literal{..} =
        Literal { _annotation = peelA _annotation
                , _predicate  = peel  _predicate
                , ..}