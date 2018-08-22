{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Exalog.SemiNaive where

import Protolude hiding (head)

import           Data.Function (id)
import           Data.List (concatMap, span, lookup)
import           Data.Maybe (mapMaybe, fromJust)
import           Data.Singletons.Decide (Decision(..), (%~))

import           Control.Comonad (Comonad(..))

import           Util.Vector
import qualified Util.List.Zipper as LZ

import           Language.Exalog.Core
import qualified Language.Exalog.Relation as R
import           Language.Exalog.Stratification (stratify)
import qualified Language.Exalog.Tuples as T

semiNaive :: forall a. Eq (PredicateAnn a)
          => R.Solution a -> Program a -> IO (R.Solution a)
semiNaive edb pr = do
  initEDB <- initEDBM
  sol <- (`fix` initEDB) $ \f sol -> do
    betterSol <- step sol
    if areAllDeltaEmpty betterSol
      then return betterSol
      else f betterSol
  return $ R.rename peel sol
  where
  (simpleClss, intensionalClss) = span isSimpleClause (clauses pr)

  initEDBM :: IO (R.Solution ('ADelta a))
  initEDBM = genSolDelta pr <$> runAndUpdateClauses simpleClss edb

  intentionals :: [ PredicateBox a ]
  intentionals = findIntentionals pr

  -- A clause is simple if it does not reference any IDB predicates in its
  -- body.
  isSimpleClause :: Clause a -> Bool
  isSimpleClause Clause{body = body} =
    not . any ((`elem` intentionals) . predicateBox) $ body

  deltaPr :: Program ('ADelta a)
  deltaPr = genProgDelta pr

  -- Adds the current deltas to the normal version of the relation.
  updateFromDelta :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  updateFromDelta edb = foldr updateFromDelta' edb intentionals

  updateFromDelta' :: PredicateBox a
                   -> R.Solution ('ADelta a)
                   -> R.Solution ('ADelta a)
  updateFromDelta' predBox@(PredicateBox p) edb =
    case R.findTuples edb (mkADelta' Delta p) of
      Just ts -> R.add (R.relation (mkADelta' Normal p) ts) edb
      Nothing ->
        panic "Impossible: couldn't find delta of an intentional predicate."

  -- Sets PrevX2 to Prev, Prev to Normal
  shiftPrevs :: R.Solution ('ADelta a)
             -> R.Solution ('ADelta a)
  shiftPrevs edb = _ $ (`R.rename` edb) $ \p ->
    case decor p of
      Normal -> updateDecor Prev   p
      Prev   -> updateDecor PrevX2 p
      _      -> p

  filterPrevX2 :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  filterPrevX2 sol = (`R.filter` sol) $ (/= PrevX2) . decor . fst

  runDelta :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  runDelta = runAndUpdateClauses (clauses deltaPr)

  areAllDeltaEmpty :: R.Solution ('ADelta a) -> Bool
  areAllDeltaEmpty =
      R.isEmpty
    . R.filter (\(p,ts) -> decor p == Delta && (not . T.isEmpty) ts)

  step :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  step = runDelta . updateFromDelta . shiftPrevs . filterPrevX2

runAndUpdateClauses :: Eq (PredicateAnn a)
                    => [ Clause a ] -> R.Solution a -> IO (R.Solution a)
runAndUpdateClauses clss edb = foldr R.add edb <$> mapM (execClause edb) clss

execClause :: forall a. Eq (PredicateAnn a)
           => R.Solution a -> Clause a -> IO (R.Relation a)
execClause edb Clause{..} = deriveHead <$> foldrM walkBody [] body
  where
  deriveHead :: [ Unifier ] -> R.Relation a
  deriveHead unifiers
    | Literal{predicate = pred, terms = terms} <- head =
      R.relation pred (mapMaybe (substitute terms) unifiers)

  walkBody :: Literal a -> [ Unifier ] -> IO [ Unifier ]
  walkBody lit unifiers = do
    unifierExtensions <- execLiteral edb lit
    return $ do
      extension <- unifierExtensions
      unifier <- unifiers
      let extendedUnifier = extension `extends` unifier
      guard (isJust extendedUnifier)
      return $ fromJust extendedUnifier

execLiteral :: Eq (PredicateAnn a)
            => R.Solution a -> Literal a -> IO [ Unifier ]
execLiteral edb lit@Literal{predicate = p@Predicate{nature = nature}, ..}
  | Extralogical action <- nature = either panic id <$> action terms
  | otherwise = return $
      maybe (panic "The predicate is not known to the Datalog engine.")
            (mapMaybe (unify terms))
            (R.findTuples edb p)

extends :: Unifier -> Unifier -> Maybe Unifier
extends [] u' = Just u'
extends (binding@(v,s) : u) u' =
  case v `lookup` u' of
    Just s' -> if s == s' then extends u u' else Nothing
    Nothing -> (binding:) <$> extends u u'

substitute :: Vector n Term -> Unifier -> Maybe (Vector n Sym)
substitute (t ::: ts) unifier
  | TSym s <- t = (s :::) <$> substitute ts unifier
  | TVar v <- t =
    case v `lookup` unifier of
      Just s  -> (s :::) <$> substitute ts unifier
      Nothing -> Nothing

unify :: Vector n Term -> Vector n Sym -> Maybe Unifier
unify Nil Nil = Just []
unify (TVar v ::: ts) (s ::: ss) = ((v,s):) <$> unify ts ss
unify (TSym s ::: ts) (s' ::: ss)
  | s == s' = unify ts ss
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Generating delta versions of predicates
--------------------------------------------------------------------------------

data Decor = Normal | Delta | Prev | PrevX2 deriving (Eq)

data    instance PredicateAnn ('ADelta a) = PredADelta Decor (PredicateAnn a)
data    instance LiteralAnn ('ADelta a)   = LitADelta (LiteralAnn a)
newtype instance ClauseAnn  ('ADelta a)   = ClADelta (ClauseAnn a)
newtype instance ProgramAnn ('ADelta a)   = ProgADelta (ProgramAnn a)

updateDecor :: Decor -> Predicate n ('ADelta a) -> Predicate n ('ADelta a)
updateDecor decor Predicate{annotation = PredADelta _ prevAnn} =
  Predicate{annotation = PredADelta decor prevAnn, ..}

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
genProgDelta :: forall a. Eq (PredicateBox a)
             => Program a -> Program ('ADelta a)
genProgDelta pr@(Program ann cs) = Program (decorA ann) (concatMap mkCls cs)
  where
  intentionals = findIntentionals pr

  mkCls :: Clause a -> [ Clause ('ADelta a) ]
  mkCls Clause{..} =
      fmap (Clause (decorA annotation) (mkADelta Delta head) . LZ.toNonEmptyList)
    . mapMaybe processBody
    . LZ.toList
    . duplicate
    . LZ.fromNonEmptyList $ body

  processBody :: LZ.Zipper (Literal a)
              -> Maybe (LZ.Zipper (Literal ('ADelta a)))
  processBody lits
    | (`elem` intentionals) . predicateBox . LZ.focus $ lits = Just
      . LZ.threeWayMap (mkPrev Prev) (mkADelta Delta) (mkPrev PrevX2) $ lits
    | otherwise = Nothing

  mkPrev :: Decor -> Literal a -> Literal ('ADelta a)
  mkPrev deco lit
    | predicateBox lit `elem` intentionals = mkADelta deco lit
    | otherwise = mkADelta Normal lit

mkADelta :: Decor -> Literal a -> Literal ('ADelta a)
mkADelta deco Literal{..} =
  Literal { annotation = decorA annotation
          , predicate  = mkADelta' deco predicate
          , ..}

mkADelta' :: Decor -> Predicate n a -> Predicate n ('ADelta a)
mkADelta' deco Predicate{..} = Predicate
  { annotation = PredADelta deco annotation
  , ..}

genSolDelta :: Eq (PredicateAnn a)
            => Program a -> R.Solution a -> R.Solution ('ADelta a)
genSolDelta pr sol =
  foldr (\(PredicateBox p) -> R.add . R.empty $ p) deltaRenamed normals
  where
  intentionals = findIntentionals pr

  normals = flip map intentionals $ \case
    PredicateBox p -> PredicateBox $ mkADelta' Normal p

  deltaRenamed = (`R.rename` sol) $ \p ->
    if PredicateBox p `elem` intentionals
      then mkADelta' Delta  p
      else mkADelta' Normal p
