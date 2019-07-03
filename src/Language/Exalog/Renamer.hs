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
  , mkPredicateMap, mkLiteralMap, mkClauseMap
  , PredicateIDMap, LiteralIDMap, ClauseIDMap
  , PredicateID(..), LiteralID(..), ClauseID(..)
  , PredicateAnn(PredARename), LiteralAnn(LitARename), ClauseAnn(ClARename), ProgramAnn(ProgARename)
  , HasPredicateID(..), HasLiteralID(..), HasClauseID(..)
  ) where

import Protolude

import qualified Data.Bimap as BM
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import           Language.Exalog.Core
import           Language.Exalog.Logger
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SrcLoc

newtype PredicateID = PredicateID Int deriving (Eq, Ord, Show)
newtype LiteralID   = LiteralID   Int deriving (Eq, Ord, Show)
newtype ClauseID    = ClauseID    Int deriving (Eq, Ord, Show)

data    instance PredicateAnn ('ARename a) = PredARename { _predicateID :: PredicateID, _prevAnn :: PredicateAnn a }
data    instance LiteralAnn   ('ARename a) = LitARename  { _literalID   :: LiteralID  , _prevAnn :: LiteralAnn   a }
data    instance ClauseAnn    ('ARename a) = ClARename   { _clauseID    :: ClauseID   , _prevAnn :: ClauseAnn    a }
newtype instance ProgramAnn   ('ARename a) = ProgARename {                              _prevAnn :: ProgramAnn   a }

type PredicateIDMap ann = BM.Bimap (PredicateBox ('ARename ann)) PredicateID
type LiteralIDMap ann   = BM.Bimap (Literal      ('ARename ann)) LiteralID
type ClauseIDMap ann    = BM.Bimap (Clause       ('ARename ann)) ClauseID

--------------------------------------------------------------------------------
-- Accessor
--------------------------------------------------------------------------------

class    HasPredicateID a                             where predicateID :: a -> PredicateID
instance HasPredicateID (PredicateAnn ('ARename ann)) where predicateID PredARename{..} = _predicateID
instance HasPredicateID (Predicate n  ('ARename ann)) where predicateID Predicate{..}   = _predicateID annotation

instance HasPredicateID (PredicateBox ('ARename ann)) where predicateID (PredicateBox pred) = predicateID pred

instance HasPredicateID (Literal      ('ARename ann)) where predicateID Literal{..} = predicateID predicate

class    HasLiteralID a                           where literalID :: a -> LiteralID
instance HasLiteralID (LiteralAnn ('ARename ann)) where literalID LitARename{..} = _literalID
instance HasLiteralID (Literal    ('ARename ann)) where literalID Literal{..}    = _literalID annotation

class    HasClauseID a                          where clauseID :: a -> ClauseID
instance HasClauseID (ClauseAnn ('ARename ann)) where clauseID ClARename{..} = _clauseID
instance HasClauseID (Clause    ('ARename ann)) where clauseID Clause{..}    = _clauseID annotation

--------------------------------------------------------------------------------
-- Renamer
--------------------------------------------------------------------------------

rename :: SpannableAnn (PredicateAnn ann)
       => IdentifiableAnn (PredicateAnn ann) b
       => Ord b
       => (Program ann, R.Solution ann)
       -> Logger (Program ('ARename ann), R.Solution ('ARename ann))
rename (pr,sol) = evalRename preds $
  (,) <$> renameProgram pr <*> renameSolution sol
  where
  preds = S.fromList $ predicates pr <> R.predicates sol

renameSolution :: SpannableAnn (PredicateAnn ann)
               => IdentifiableAnn (PredicateAnn ann) a
               => Ord a
               => R.Solution ann
               -> Rename ann (R.Solution ('ARename ann))
renameSolution = R.renameM renamePredicate

renameProgram :: SpannableAnn (PredicateAnn ann)
              => IdentifiableAnn (PredicateAnn ann) a
              => Ord a
              => Program ann
              -> Rename ann (Program ('ARename ann))
renameProgram Program{..} = do
  renamedStrata     <- traverse (stratumOverF $ traverse renameClause) strata
  renamedQueryPreds <- traverse (\(PredicateBox pred) -> PredicateBox <$> renamePredicate pred) queryPreds
  pure Program
    { annotation = ProgARename annotation
    , strata     = renamedStrata
    , queryPreds = renamedQueryPreds
    , ..}

renameClause :: SpannableAnn (PredicateAnn ann)
             => IdentifiableAnn (PredicateAnn ann) b
             => Ord b
             => Clause ann
             -> Rename ann (Clause ('ARename ann))
renameClause Clause{..} = do
  renamedHead <- renameLiteral head
  renamedBody <- traverse renameLiteral body
  id <- freshID
  pure Clause
    { annotation = ClARename (ClauseID id) annotation
    , head       = renamedHead
    , body       = renamedBody
    , ..}

renameLiteral :: SpannableAnn (PredicateAnn ann)
              => IdentifiableAnn (PredicateAnn ann) b
              => Ord b
              => Literal ann
              -> Rename ann (Literal ('ARename ann))
renameLiteral Literal{..} = do
  renamedPredicate <- renamePredicate predicate
  id <- freshID
  pure $ Literal
    { annotation = LitARename (LiteralID id) annotation
    , predicate  = renamedPredicate
    , ..}

renamePredicate :: SpannableAnn (PredicateAnn ann)
                => IdentifiableAnn (PredicateAnn ann) b
                => Ord b
                => Predicate n ann
                -> Rename ann (Predicate n ('ARename ann))
renamePredicate pred@Predicate{..} = do
  preds <- ask
  case PredicateBox pred `S.lookupIndex` preds of
    Just ix -> pure $
      Predicate{annotation = PredARename (PredicateID ix) annotation,..}
    Nothing -> lift $ lift $ scream (Just $ span pred)
      "Impossible happened! Renamed predicate is not a predicate of the program."

mkPredicateMap :: IdentifiableAnn (PredicateAnn ann) a
               => Ord a
               => Program ('ARename ann)
               -> PredicateIDMap ann
mkPredicateMap pr = BM.fromList $ (<$> predicates pr) $
  \pBox@(PredicateBox Predicate{..}) -> (pBox, predicateID annotation)

mkLiteralMap :: IdentifiableAnn (PredicateAnn ann) a
             => IdentifiableAnn (LiteralAnn ann) b
             => Ord a => Ord b
             => Program ('ARename ann)
             -> LiteralIDMap ann
mkLiteralMap Program{..} = BM.fromList
                         $ fmap (\lit@Literal{..} -> (lit, literalID annotation))
                         . join
                         $ NE.toList . literals
                       <$> join (map _unStratum strata)

mkClauseMap :: IdentifiableAnn (PredicateAnn ann) a
            => IdentifiableAnn (LiteralAnn ann) b
            => IdentifiableAnn (ClauseAnn ann) c
            => Ord a => Ord b => Ord c
            => Program ('ARename ann)
            -> ClauseIDMap ann
mkClauseMap Program{..} = BM.fromList $ (<$> join (map _unStratum strata)) $
  \cl@Clause{..} -> (cl, clauseID annotation)

--------------------------------------------------------------------------------
-- Monadic actions for renaming
--------------------------------------------------------------------------------

type IDCounterT = StateT Int
type Rename ann = ReaderT (S.Set (PredicateBox ann)) (IDCounterT Logger)

evalRename :: S.Set (PredicateBox ann) -> Rename ann a -> Logger a
evalRename preds = (`evalStateT` 0) . (`runReaderT` preds)

freshID :: Rename ann Int
freshID = lift $ do
  id <- get
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

instance IdentifiableAnn (PredicateAnn ann) b => IdentifiableAnn (PredicateAnn ('ARename ann)) Int where
  idFragment (PredARename (PredicateID id) rest) = id
instance IdentifiableAnn (LiteralAnn   ann) b => IdentifiableAnn (LiteralAnn   ('ARename ann)) Int where
  idFragment (LitARename  (LiteralID   id) rest) = id
instance IdentifiableAnn (ClauseAnn    ann) b => IdentifiableAnn (ClauseAnn    ('ARename ann)) Int where
  idFragment (ClARename   (ClauseID id)    rest) = id
instance IdentifiableAnn (ProgramAnn   ann) b => IdentifiableAnn (ProgramAnn   ('ARename ann)) b where
  idFragment (ProgARename                  rest) = idFragment rest

instance PeelableAnn PredicateAnn 'ARename where peelA (PredARename _ prevAnn) = prevAnn
instance PeelableAnn LiteralAnn   'ARename where peelA (LitARename  _ prevAnn) = prevAnn
instance PeelableAnn ClauseAnn    'ARename where peelA (ClARename   _ prevAnn) = prevAnn
instance PeelableAnn ProgramAnn   'ARename where peelA (ProgARename   prevAnn) = prevAnn

instance PeelableAST (Literal ('ARename ann)) where
  peel Literal{..} = Literal
    { annotation = peelA annotation
    , predicate = peel predicate
    , ..}
