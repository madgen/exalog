{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Delta
  ( Decor(..)
  , updateDecor
  , decor
  , elimDecor
  , mkDeltaPredicate
  , mkDeltaLiteral
  , mkDeltaProgram
  , mkDeltaSolution
  , cleanDeltaSolution
  ) where

import Protolude

import           Control.Comonad (Comonad(..))

import qualified Util.List.Zipper as LZ

import           Language.Exalog.Core
import qualified Language.Exalog.Relation as R

data Decor = Normal | Delta | Prev | PrevX2 deriving (Eq, Show)

data    instance PredicateAnn ('ADelta a) = PredADelta Decor (PredicateAnn a)
data    instance LiteralAnn ('ADelta a)   = LitADelta (LiteralAnn a)
newtype instance ClauseAnn  ('ADelta a)   = ClADelta (ClauseAnn a)
newtype instance ProgramAnn ('ADelta a)   = ProgADelta (ProgramAnn a)

deriving instance Show (PredicateAnn a) => Show (PredicateAnn ('ADelta a))

updateDecor :: Decor -> Predicate n ('ADelta a) -> Predicate n ('ADelta a)
updateDecor decor p@Predicate{annotation = PredADelta _ prevAnn} =
  p {annotation = PredADelta decor prevAnn}

elimDecor :: Decor -> R.Solution ('ADelta a) -> R.Solution ('ADelta a)
elimDecor d sol = (`R.filter` sol) $ \(R.Relation p _) -> decor p /= d

decor :: Predicate n ('ADelta a) -> Decor
decor Predicate{annotation = PredADelta decor _} = decor

instance DecorableAnn LiteralAnn 'ADelta where
  decorA = LitADelta
instance DecorableAnn ClauseAnn 'ADelta where
  decorA = ClADelta
instance DecorableAnn ProgramAnn 'ADelta where
  decorA = ProgADelta

instance PeelableAnn PredicateAnn 'ADelta where
  peelA (PredADelta _ prevAnn) = prevAnn

deriving instance Eq (PredicateAnn a) => Eq (PredicateAnn ('ADelta a))

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
mkDeltaProgram :: forall a. Eq (PredicateBox a)
               => Program a -> Program ('ADelta a)
mkDeltaProgram pr@(Program ann cs) = Program (decorA ann) (concatMap mkCls cs)
  where
  intentionals = findIntentionals pr

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
    | (`elem` intentionals) . predicateBox . LZ.focus $ lits = Just
      . LZ.threeWayMap (mkPrev Prev) (mkDeltaLiteral Delta) (mkPrev PrevX2) $ lits
    | otherwise = Nothing

  mkPrev :: Decor -> Literal a -> Literal ('ADelta a)
  mkPrev deco lit
    | predicateBox lit `elem` intentionals = mkDeltaLiteral deco lit
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

mkDeltaSolution :: Eq (PredicateAnn a)
                => Program a -> R.Solution a -> R.Solution ('ADelta a)
mkDeltaSolution pr sol =
  foldr (R.add . ((`R.Relation` mempty) $$)) deltaRenamed normals
  where
  intentionals = findIntentionals pr

  normals = flip map intentionals $ \case
    PredicateBox p -> PredicateBox $ mkDeltaPredicate Normal p

  deltaRenamed = (`R.rename` sol) $ \p ->
    if PredicateBox p `elem` intentionals
      then mkDeltaPredicate Delta  p
      else mkDeltaPredicate Normal p

cleanDeltaSolution :: R.Solution ('ADelta a) -> R.Solution a
cleanDeltaSolution =
  R.rename peel . R.filter (\(R.Relation p _) -> decor p == Normal)
