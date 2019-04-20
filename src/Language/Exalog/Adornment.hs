{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Adornment
  ( adornProgram
  , adornClauses
  , adornClause
  ) where

import Protolude hiding (head)

import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (fromSing)
import qualified Data.Vector.Sized as V

import Language.Exalog.Core

data Adornment = Free | Bound deriving (Eq, Ord, Show)

newtype instance PredicateAnn ('AAdornment ann) = PredAAdornment               (PredicateAnn ann)
data    instance LiteralAnn   ('AAdornment ann) = LitAAdornment  [ Adornment ] (LiteralAnn   ann)
newtype instance ClauseAnn    ('AAdornment ann) = ClAAdornment                 (ClauseAnn    ann)
newtype instance ProgramAnn   ('AAdornment ann) = ProgAAdornment               (ProgramAnn   ann)

instance DecorableAnn PredicateAnn 'AAdornment where decorA = PredAAdornment
instance DecorableAnn ClauseAnn    'AAdornment where decorA = ClAAdornment
instance DecorableAnn ProgramAnn   'AAdornment where decorA = ProgAAdornment

instance PeelableAnn PredicateAnn 'AAdornment where peelA (PredAAdornment ann)  = ann
instance PeelableAnn LiteralAnn 'AAdornment   where peelA (LitAAdornment _ ann) = ann

instance IdentifiableAnn (PredicateAnn ann) b
    => IdentifiableAnn (PredicateAnn ('AAdornment ann)) b where
  idFragment (PredAAdornment rest) = idFragment rest
instance IdentifiableAnn (LiteralAnn ann) b
    => IdentifiableAnn (LiteralAnn ('AAdornment ann)) (b, [ Adornment ]) where
  idFragment (LitAAdornment ads rest) = (idFragment rest, ads)
instance IdentifiableAnn (ClauseAnn ann) b
    => IdentifiableAnn (ClauseAnn ('AAdornment ann)) b where
  idFragment (ClAAdornment rest) = idFragment rest
instance IdentifiableAnn (ProgramAnn ann) b
    => IdentifiableAnn (ProgramAnn ('AAdornment ann)) b where
  idFragment (ProgAAdornment rest) = idFragment rest

--------------------------------------------------------------------------------
-- Program adornment
--------------------------------------------------------------------------------

adornProgram :: Identifiable (PredicateAnn ann) b
             => Program ann -> Program ('AAdornment ann)
adornProgram Program{..} = Program
  { annotation = decorA annotation
  , clauses    = adornedClauses
  , queryPreds = (PredicateBox . decorate $$) <$> queryPreds
  , ..}
  where
  adornedClauses = join $ (`adornClauses` clauses) <$> queryPreds

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
  let targets = map target . join $ NE.toList . body <$> clauses

  modify (\s -> s { _adornedClauses = clauses <> _adornedClauses s
                  , _toAdorn        = nub $ targets <> _toAdorn s
                  })
  where
  target :: Literal ('AAdornment ann) -> (PredicateBox ann, [ Adornment ])
  target l@Literal{annotation = LitAAdornment ads _} =
    (PredicateBox . peel $$ predicateBox l, ads)

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
  allFreeBinding = replicate (fromIntegral (fromSing (arity p))) $ Free

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
              [ cl | cl@Clause{head = lit} <- clauses
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

adornClause :: [ Adornment ] -> Clause ann -> Clause ('AAdornment ann)
adornClause ads cl@Clause{..} =
  runAdornClause (adornClauseM cl) boundVars
  where
  boundVars = boundVariables ads head

adornClauseM :: Clause ann -> AdornClause (Clause ('AAdornment ann))
adornClauseM Clause{..} = do
  aHead <- adornLiteralM head

  aBody <- traverse adornLiteralM body

  pure $ Clause{head = aHead, body = aBody, annotation = decorA annotation}

-- Bound variables of a literal wrt a binding pattern
boundVariables :: [ Adornment ] -> Literal ann -> [ Var ]
boundVariables ads Literal{..} =
  (`mapMaybe` zip (V.toList terms) ads) $ \case
    (TVar v, Bound) -> Just v
    _               -> Nothing

--------------------------------------------------------------------------------
-- Literal adornment
--------------------------------------------------------------------------------

-- Given a binding pattern adorn a literal
adornLiteral :: [ Adornment ] -> Literal ann -> Literal ('AAdornment ann)
adornLiteral ads Literal{..} = Literal
  { annotation = LitAAdornment ads annotation
  , predicate = decorate predicate
  , ..}

adornLiteralM :: Literal ann -> AdornClause (Literal ('AAdornment ann))
adornLiteralM lit@Literal{..} = do
  ads <- deriveAdornmentM lit

  let variablesBeingBound = variables lit

  let adornedLit = adornLiteral ads lit

  modify (variablesBeingBound <>)

  pure adornedLit

-- Use the bound variables to figure out a adornment pattern for the
-- literal's terms
deriveAdornment :: Literal ann -> [ Var ] -> [ Adornment ]
deriveAdornment Literal{..} boundVars =
  (`map` V.toList terms) $ \case
    TSym{} -> Bound
    TVar v -> if v `elem` boundVars then Bound else Free

deriveAdornmentM :: Literal ann -> AdornClause [ Adornment ]
deriveAdornmentM lit = deriveAdornment lit <$> getBoundVariables

