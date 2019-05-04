{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Renamer
  ( rename
  , mkPredicateMap
  , mkLiteralMap
  , mkClauseMap
  ) where

import Protolude

import qualified Data.Bimap as BM
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Language.Exalog.Core
import Language.Exalog.Logger
import Language.Exalog.SrcLoc

data    instance PredicateAnn ('ARename a) = PredARename { _predicateID :: Int, _prevAnn :: PredicateAnn a }
data    instance LiteralAnn   ('ARename a) = LitARename  { _literalID   :: Int, _prevAnn :: LiteralAnn   a }
data    instance ClauseAnn    ('ARename a) = ClARename   { _clauseID    :: Int, _prevAnn :: ClauseAnn    a }
newtype instance ProgramAnn   ('ARename a) = ProgARename {                      _prevAnn :: ProgramAnn   a }

--------------------------------------------------------------------------------
-- Renamer
--------------------------------------------------------------------------------

rename :: SpannableAnn (PredicateAnn ann)
       => Identifiable (PredicateAnn ann) b
       => Program ann
       -> Logger (Program ('ARename ann))
rename pr = evalRename (S.fromList $ predicates pr) . renameProgram $ pr

renameProgram :: SpannableAnn (PredicateAnn ann)
              => Identifiable (PredicateAnn ann) b
              => Program ann
              -> Rename ann (Program ('ARename ann))
renameProgram Program{..} = do
  renamedClauses    <- traverse renameClause clauses
  renamedQueryPreds <- traverse (\(PredicateBox pred) -> PredicateBox <$> renamePredicate pred) queryPreds
  pure Program
    { annotation = ProgARename annotation
    , clauses    = renamedClauses
    , queryPreds = renamedQueryPreds
    , ..}

renameClause :: SpannableAnn (PredicateAnn ann)
             => Identifiable (PredicateAnn ann) b
             => Clause ann
             -> Rename ann (Clause ('ARename ann))
renameClause Clause{..} = do
  renamedHead <- renameLiteral head
  renamedBody <- traverse renameLiteral body
  id <- freshID
  pure Clause
    { annotation = ClARename id annotation
    , head       = renamedHead
    , body       = renamedBody
    , ..}

renameLiteral :: SpannableAnn (PredicateAnn ann)
              => Identifiable (PredicateAnn ann) b
              => Literal ann
              -> Rename ann (Literal ('ARename ann))
renameLiteral Literal{..} = do
  renamedPredicate <- renamePredicate predicate
  id <- freshID
  pure $ Literal
    { annotation = LitARename id annotation
    , predicate  = renamedPredicate
    , ..}

renamePredicate :: SpannableAnn (PredicateAnn ann)
                => Identifiable (PredicateAnn ann) b
                => Predicate n ann
                -> Rename ann (Predicate n ('ARename ann))
renamePredicate pred@Predicate{..} = do
  preds <- ask
  case PredicateBox pred `S.lookupIndex` preds of
    Just ix -> pure $
      Predicate{annotation = PredARename ix annotation,..}
    Nothing -> lift $ lift $ scream (Just $ span pred)
      "Impossible happened! Renamed predicate is not a predicate of the program."

mkPredicateMap :: IdentifiableAnn (PredicateAnn ann) a
               => Ord a
               => Program ('ARename ann)
               -> BM.Bimap (PredicateBox ('ARename ann)) Int
mkPredicateMap pr = BM.fromList $ (<$> predicates pr) $
  \pBox@(PredicateBox Predicate{..}) -> (pBox, _predicateID annotation)

mkLiteralMap :: IdentifiableAnn (PredicateAnn ann) a
             => IdentifiableAnn (LiteralAnn ann) b
             => Ord a => Ord b
             => Program ('ARename ann)
             -> BM.Bimap (Literal ('ARename ann)) Int
mkLiteralMap Program{..} = BM.fromList
                         $ fmap (\lit@Literal{..} -> (lit, _literalID annotation))
                         . join
                         $ NE.toList . literals
                       <$> clauses

mkClauseMap :: IdentifiableAnn (PredicateAnn ann) a
            => IdentifiableAnn (LiteralAnn ann) b
            => IdentifiableAnn (ClauseAnn ann) c
            => Ord a => Ord b => Ord c
            => Program ('ARename ann)
            -> BM.Bimap (Clause ('ARename ann)) Int
mkClauseMap Program{..} = BM.fromList $ (<$> clauses) $
  \cl@Clause{..} -> (cl, _clauseID annotation)

--------------------------------------------------------------------------------
-- Monadic actions for renaming
--------------------------------------------------------------------------------

type IDCounterT = StateT Int
type Rename ann = ReaderT (S.Set (PredicateBox ann)) (IDCounterT Logger)

evalRename :: S.Set (PredicateBox ann) -> Rename ann a -> Logger a
evalRename preds = (`evalStateT` 0) . (`runReaderT` preds)

freshID :: Rename ann Int
freshID = lift $ do
  id <- _counter <$> get
  modify (+ 1)
  pure id

--------------------------------------------------------------------------------
-- Annotation instances
--------------------------------------------------------------------------------

deriving instance Show (PredicateAnn a) => Show (PredicateAnn ('ARename a))
deriving instance Show (LiteralAnn   a) => Show (LiteralAnn   ('ARename a))
deriving instance Show (ClauseAnn    a) => Show (ClauseAnn    ('ARename a))
deriving instance Show (ProgramAnn   a) => Show (ProgramAnn   ('ARename a))

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('ARename a))
deriving instance Eq (LiteralAnn   a) => Eq (LiteralAnn   ('ARename a))
deriving instance Eq (ClauseAnn    a) => Eq (ClauseAnn    ('ARename a))
deriving instance Eq (ProgramAnn   a) => Eq (ProgramAnn   ('ARename a))

deriving instance Ord (PredicateAnn a) => Ord (PredicateAnn ('ARename a))
deriving instance Ord (LiteralAnn   a) => Ord (LiteralAnn   ('ARename a))
deriving instance Ord (ClauseAnn    a) => Ord (ClauseAnn    ('ARename a))
deriving instance Ord (ProgramAnn   a) => Ord (ProgramAnn   ('ARename a))

instance SpannableAnn (PredicateAnn a) => SpannableAnn (PredicateAnn ('ARename a)) where
  annSpan (PredARename _ ann) = annSpan ann
instance SpannableAnn (LiteralAnn   a) => SpannableAnn (LiteralAnn   ('ARename a)) where
  annSpan (LitARename  _ ann) = annSpan ann
instance SpannableAnn (ClauseAnn    a) => SpannableAnn (ClauseAnn    ('ARename a)) where
  annSpan (ClARename   _ ann) = annSpan ann
instance SpannableAnn (ProgramAnn a)   => SpannableAnn (ProgramAnn   ('ARename a)) where
  annSpan (ProgARename   ann) = annSpan ann

instance IdentifiableAnn (PredicateAnn ann) b => IdentifiableAnn (PredicateAnn ('ARename ann)) (Int,b) where
  idFragment (PredARename id rest) = (id, idFragment rest)
instance IdentifiableAnn (LiteralAnn   ann) b => IdentifiableAnn (LiteralAnn   ('ARename ann)) (Int,b) where
  idFragment (LitARename  id rest) = (id, idFragment rest)
instance IdentifiableAnn (ClauseAnn    ann) b => IdentifiableAnn (ClauseAnn    ('ARename ann)) (Int,b) where
  idFragment (ClARename   id rest) = (id, idFragment rest)
instance IdentifiableAnn (ProgramAnn   ann) b => IdentifiableAnn (ProgramAnn   ('ARename ann)) b where
  idFragment (ProgARename    rest) = idFragment rest
