{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Adornment
  ( Adornment(..)
  , adornment
  , adornProgram
  , adornClauses
  , adornClause
  , adornLiteral
  ) where

import Protolude hiding (head)

import           Data.List (nub, (\\))
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (fromSing)
import qualified Data.Vector.Sized as V

import Text.PrettyPrint (hcat)

import Language.Exalog.Pretty.Helper (Pretty(..), prettyC)
import Language.Exalog.Core

data Adornment = Free | Bound deriving (Eq, Ord, Show)

instance Pretty Adornment where
  pretty Free  = "f"
  pretty Bound = "b"

instance Pretty [ Adornment ] where
  pretty = hcat . prettyC

instance Pretty b => Pretty ([ Adornment ], b) where
  pretty (dec, b) = pretty dec <> "_" <> pretty b

newtype instance PredicateAnn ('AAdornment ann) = PredAAdornment               (PredicateAnn  ann)
data    instance LiteralAnn   ('AAdornment ann) = LitAAdornment  [ Adornment ] (LiteralAnn    ann)
newtype instance ClauseAnn    ('AAdornment ann) = ClAAdornment                 (ClauseAnn     ann)
newtype instance ProgramAnn   ('AAdornment ann) = ProgAAdornment               (ProgramAnn    ann)
newtype instance KnowledgeAnn ('AAdornment ann) = KnowAAdornment               (KnowledgeAnn  ann)

deriving instance Show (PredicateAnn a)  => Show (PredicateAnn ('AAdornment a))
deriving instance Show (LiteralAnn a)    => Show (LiteralAnn   ('AAdornment a))
deriving instance Show (ClauseAnn a)     => Show (ClauseAnn    ('AAdornment a))
deriving instance Show (ProgramAnn a)    => Show (ProgramAnn   ('AAdornment a))
deriving instance Show (KnowledgeAnn a)  => Show (KnowledgeAnn ('AAdornment a))

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('AAdornment a))
deriving instance Eq (LiteralAnn a)   => Eq (LiteralAnn   ('AAdornment a))
deriving instance Eq (ClauseAnn a)    => Eq (ClauseAnn    ('AAdornment a))
deriving instance Eq (ProgramAnn a)   => Eq (ProgramAnn   ('AAdornment a))
deriving instance Eq (KnowledgeAnn a) => Eq (KnowledgeAnn ('AAdornment a))

deriving instance Ord (PredicateAnn a) => Ord (PredicateAnn  ('AAdornment a))
deriving instance Ord (LiteralAnn a)   => Ord (LiteralAnn    ('AAdornment a))
deriving instance Ord (ClauseAnn a)    => Ord (ClauseAnn     ('AAdornment a))
deriving instance Ord (ProgramAnn a)   => Ord (ProgramAnn    ('AAdornment a))
deriving instance Ord (KnowledgeAnn a) => Ord (KnowledgeAnn  ('AAdornment a))

instance DecorableAnn PredicateAnn 'AAdornment where decorA = PredAAdornment
instance DecorableAnn ClauseAnn    'AAdornment where decorA = ClAAdornment
instance DecorableAnn ProgramAnn   'AAdornment where decorA = ProgAAdornment

instance PeelableAnn PredicateAnn 'AAdornment where peelA (PredAAdornment ann)  = ann
instance PeelableAnn LiteralAnn   'AAdornment where peelA (LitAAdornment _ ann) = ann
instance PeelableAnn KnowledgeAnn 'AAdornment where peelA (KnowAAdornment ann)  = ann

instance IdentifiableAnn (PredicateAnn ann) b
    => IdentifiableAnn (PredicateAnn ('AAdornment ann)) b where
  idFragment (PredAAdornment rest) = idFragment rest
instance IdentifiableAnn (LiteralAnn ann) b
    => IdentifiableAnn (LiteralAnn ('AAdornment ann)) ([ Adornment ], b) where
  idFragment (LitAAdornment ads rest) = (ads, idFragment rest)
instance IdentifiableAnn (ClauseAnn ann) b
    => IdentifiableAnn (ClauseAnn ('AAdornment ann)) b where
  idFragment (ClAAdornment rest) = idFragment rest
instance IdentifiableAnn (ProgramAnn ann) b
    => IdentifiableAnn (ProgramAnn ('AAdornment ann)) b where
  idFragment (ProgAAdornment rest) = idFragment rest
instance IdentifiableAnn (KnowledgeAnn ann) b
    => IdentifiableAnn (KnowledgeAnn ('AAdornment ann)) b where
  idFragment (KnowAAdornment rest) = idFragment rest

instance SpannableAnn (PredicateAnn a) => SpannableAnn (PredicateAnn ('AAdornment a)) where
  annSpan (PredAAdornment ann) = annSpan ann
instance SpannableAnn (LiteralAnn a) => SpannableAnn (LiteralAnn ('AAdornment a)) where
  annSpan (LitAAdornment _ ann) = annSpan ann
instance SpannableAnn (ClauseAnn a) => SpannableAnn (ClauseAnn ('AAdornment a)) where
  annSpan (ClAAdornment ann) = annSpan ann
instance SpannableAnn (ProgramAnn a) => SpannableAnn (ProgramAnn ('AAdornment a)) where
  annSpan (ProgAAdornment ann) = annSpan ann
instance SpannableAnn (KnowledgeAnn a) => SpannableAnn (KnowledgeAnn ('AAdornment a)) where
  annSpan (KnowAAdornment ann) = annSpan ann

--------------------------------------------------------------------------------
-- Accessor to the binding pattern
--------------------------------------------------------------------------------

adornment :: Literal ('AAdornment ann) -> [ Adornment ]
adornment Literal{_annotation = LitAAdornment ads _} = ads

--------------------------------------------------------------------------------
-- Program adornment
--------------------------------------------------------------------------------

adornProgram :: ( Identifiable (PredicateAnn ann) b
                , Identifiable (LiteralAnn ann) b
                , Identifiable (ClauseAnn ann) b
                )
             => Program ann -> Program ('AAdornment ann)
adornProgram Program{..} = Program
  { _annotation = decorA _annotation
  , _strata     = adornedStrata
  , _queries    = (PredicateBox . decorate $$) <$> _queries
  , ..}
  where
  adornedStrata = do
    Stratum cls <- _strata
    pure $ Stratum . nub $ (`adornClauses` cls) =<< _queries

--------------------------------------------------------------------------------
-- Multiple clause adornment with an entry point
--------------------------------------------------------------------------------

data AdornState ann = AdornState
  { _toAdorn        :: [ (PredicateBox ann, [ Adornment ]) ]
  , _alreadyAdorned :: [ (PredicateBox ann, [ Adornment ]) ]
  , _adornedClauses :: [ Clause ('AAdornment ann) ]
  }

type Adorn ann = State (AdornState ann)

-- Poll predicate binding pattern pair that needs to be adorned
pollToAdorn :: Adorn ann (Maybe (PredicateBox ann, [ Adornment ]))
pollToAdorn = do
  toAdorn <- _toAdorn <$> get

  case toAdorn of
    (t : ts) -> do
      modify (\s -> s {_toAdorn = ts, _alreadyAdorned = t : _alreadyAdorned s})
      pure $ Just t
    [] -> pure Nothing

addAdornedClauses :: Identifiable (PredicateAnn ann) b
                  => [ Clause ('AAdornment ann) ] -> Adorn ann ()
addAdornedClauses clauses = do
  let targets = map target . join $ NE.toList . _body <$> clauses

  modify (\s -> s { _adornedClauses = clauses <> _adornedClauses s
                  , _toAdorn        =
                    nub $ (targets \\ _alreadyAdorned s) <> _toAdorn s
                  })
  where
  target :: Literal ('AAdornment ann) -> (PredicateBox ann, [ Adornment ])
  target lit@Literal{_annotation = LitAAdornment ads _} =
    (PredicateBox . peel $$ predicateBox lit, ads)

execAdorn :: Adorn ann a
          -> PredicateBox ann
          -> [ Adornment ]
          -> [ Clause ('AAdornment ann) ]
execAdorn action pBox ads =
  _adornedClauses $ execState action (AdornState [ (pBox, ads) ] [ ] [ ])

adornClauses :: Identifiable (PredicateAnn ann) b
             => PredicateBox ann
             -> [ Clause ann ]
             -> [ Clause ('AAdornment ann) ]
adornClauses pBox@(PredicateBox p) clauses =
  execAdorn (adornClausesM clauses) pBox allFreeBinding
  where
  allFreeBinding = replicate (fromIntegral . fromSing . _arity $ p) Free

adornClausesM :: forall ann b. Identifiable (PredicateAnn ann) b
              => [ Clause ann ] -> Adorn ann ()
adornClausesM clauses = go
  where
  go :: Adorn ann ()
  go = do
    mToAdorn <- pollToAdorn

    case mToAdorn of
      Just (pBox, ads) -> do
        let clausesToAdorn =
              [ cl | cl@Clause{_head = lit} <- clauses
                   , predicateBox lit == pBox ]

        let adornedClauses = map (adornClause ads) clausesToAdorn

        addAdornedClauses adornedClauses
        go
      Nothing -> pure ()

--------------------------------------------------------------------------------
-- Clause adornment
--------------------------------------------------------------------------------

-- Keeps track of bound variables of the clause
type AdornClause = State [ Var ]

runAdornClause :: AdornClause a -> [ Var ] -> a
runAdornClause = evalState

getBoundVariables :: AdornClause [ Var ]
getBoundVariables = get

addBoundVariables :: [ Var ] -> AdornClause ()
addBoundVariables vars = modify (vars <>)

adornClause :: [ Adornment ] -> Clause ann -> Clause ('AAdornment ann)
adornClause ads cl@Clause{..} =
  runAdornClause (adornClauseM cl) boundVars
  where
  boundVars = boundVariables ads _head

adornClauseM :: Clause ann -> AdornClause (Clause ('AAdornment ann))
adornClauseM Clause{..} = do
  aHead <- adornLiteralM _head

  aBody <-traverse
    (\lit -> adornLiteralM lit <* addBoundVariables (variables lit)) _body

  pure $ Clause{_head = aHead, _body = aBody, _annotation = decorA _annotation}

-- Bound variables of a literal wrt a binding pattern
boundVariables :: [ Adornment ] -> Literal ann -> [ Var ]
boundVariables ads Literal{..} =
  (`mapMaybe` zip (V.toList _terms) ads) $ \case
    (TVar v, Bound) -> Just v
    _               -> Nothing

--------------------------------------------------------------------------------
-- Literal adornment
--------------------------------------------------------------------------------

-- Given a binding pattern adorn a literal
adornLiteral :: [ Adornment ] -> Literal ann -> Literal ('AAdornment ann)
adornLiteral ads Literal{..} = Literal
  { _annotation = LitAAdornment ads _annotation
  , _predicate  = decorate _predicate
  , ..}

adornLiteralM :: Literal ann -> AdornClause (Literal ('AAdornment ann))
adornLiteralM lit@Literal{..} = do
  ads <- deriveAdornmentM lit

  let adornedLit = adornLiteral ads lit

  pure adornedLit

-- Use the bound variables to figure out a adornment pattern for the
-- literal's terms
deriveAdornment :: Literal ann -> [ Var ] -> [ Adornment ]
deriveAdornment Literal{..} boundVars =
  (`map` V.toList _terms) $ \case
    TSym{} -> Bound
    TWild  -> Free
    TVar v -> if v `elem` boundVars then Bound else Free

deriveAdornmentM :: Literal ann -> AdornClause [ Adornment ]
deriveAdornmentM lit = deriveAdornment lit <$> getBoundVariables

