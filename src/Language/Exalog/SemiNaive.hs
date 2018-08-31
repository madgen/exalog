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

module Language.Exalog.SemiNaive where

import Protolude hiding (head, pred)

import           Data.Function (id)
import           Data.List (concatMap, lookup)
import           Data.Maybe (mapMaybe, catMaybes, fromJust)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Delta
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
  return $ cleanDeltaSolution sol
  where
  -- A simple clause is one without references to IDB predicates in its body.
  simpleClss = flip filter (clauses pr) $ \Clause{body = body} ->
    not . any ((`elem` intentionals) . predicateBox) $ body

  initEDBM :: IO (R.Solution ('ADelta a))
  initEDBM = do
    rels <- runClauses simpleClss edb
    let deltaSol = mkDeltaSolution pr . foldr R.add edb $ rels
    return $ (`R.atEach` deltaSol) $ \case
      (p, ts)
        | Prev   <- decor p -> R.findTuples deltaSol (updateDecor Delta p)
        | PrevX2 <- decor p -> R.findTuples deltaSol (updateDecor Delta p)
        | otherwise -> ts

  intentionals :: [ PredicateBox a ]
  intentionals = findIntentionals pr

  deltaPr :: Program ('ADelta a)
  deltaPr = mkDeltaProgram pr

  -- Adds the current deltas to the normal version of the relation.
  -- Basicall S_{i+1} = S_i \cup delta S_i
  updateFromDelta :: R.Solution ('ADelta a) -> R.Solution ('ADelta a)
  updateFromDelta edb = foldr updateFromDelta' edb intentionals

  updateFromDelta' :: PredicateBox a
                   -> R.Solution ('ADelta a)
                   -> R.Solution ('ADelta a)
  updateFromDelta' (PredicateBox p) edb =
    let ts = R.findTuples edb (mkDeltaPredicate Delta p)
        tsPrev = R.findTuples edb (mkDeltaPredicate Prev p)
    in R.add (R.Relation (mkDeltaPredicate Normal p) (ts <> tsPrev)) edb

  -- Sets PrevX2 to Prev, Prev to Normal
  shiftPrevs :: R.Solution ('ADelta a)
             -> R.Solution ('ADelta a)
  shiftPrevs edb = (`R.rename` edb) $ \p ->
    -- This is stupidly inefficient
    if PredicateBox (peel p) `elem` intentionals
      then
        case decor p of
          Normal -> updateDecor Prev   p
          Prev   -> updateDecor PrevX2 p
          _      -> p
      else
        p

  runDelta :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  runDelta edb = axeDeltaRedundancies
               . foldr R.add edb
             <$> runClauses (clauses deltaPr) edb

  axeDeltaRedundancies :: R.Solution ('ADelta a)
                       -> R.Solution ('ADelta a)
  axeDeltaRedundancies edb = (`R.atEach` edb) $ \(p, ts) ->
    case decor p of
      Delta -> ts `T.difference` R.findTuples edb (updateDecor Normal p)
      _ -> ts

  areAllDeltaEmpty :: R.Solution ('ADelta a) -> Bool
  areAllDeltaEmpty =
      R.isEmpty
    . R.filter (\(R.Relation p ts) -> decor p == Delta && (not . T.isEmpty) ts)

  step :: R.Solution ('ADelta a) -> IO (R.Solution ('ADelta a))
  step = runDelta . updateFromDelta . shiftPrevs . elimDecor PrevX2

runClauses :: Eq (PredicateAnn a)
           => [ Clause a ] -> R.Solution a -> IO [ R.Relation a ]
runClauses clss edb = mapM (execClause edb) clss

execClause :: forall a. Eq (PredicateAnn a)
           => R.Solution a -> Clause a -> IO (R.Relation a)
execClause edb Clause{..} = deriveHead <$> foldrM walkBody [] body
  where
  deriveHead :: [ Unifier ] -> R.Relation a
  deriveHead unifiers
    | Literal{predicate = pred, terms = terms} <- head =
      R.Relation pred (T.fromList $ mapMaybe (substitute terms) unifiers)

  walkBody :: Literal a -> [ Unifier ] -> IO [ Unifier ]
  walkBody lit unifiers = do
    unifierExtensions <- execLiteral edb lit
    return $ do
      extension <- unifierExtensions
      if null unifiers
        then return extension
        else do
          unifier <- unifiers
          let extendedUnifier = extension `extends` unifier
          guard (isJust extendedUnifier)
          return $ fromJust extendedUnifier

execLiteral :: Eq (PredicateAnn a)
            => R.Solution a -> Literal a -> IO [ Unifier ]
execLiteral edb Literal{predicate = p@Predicate{nature = nature}, ..}
  | Extralogical action <- nature = either panic id <$> action terms
  | otherwise = return $ mapMaybe (unify terms) (T.toList $ R.findTuples edb p)

extends :: Unifier -> Unifier -> Maybe Unifier
extends [] u' = Just u'
extends (binding@(v,s) : u) u' =
  case v `lookup` u' of
    Just s' -> if s == s' then extends u u' else Nothing
    Nothing -> (binding:) <$> extends u u'

substitute :: V.Vector n Term -> Unifier -> Maybe (V.Vector n Sym)
substitute v unifier = (`traverse` v) $ \case
  TSym s -> Just s
  TVar v -> v `lookup` unifier

unify :: V.Vector n Term -> V.Vector n Sym -> Maybe Unifier
unify v w = catMaybes . V.toList <$> V.zipWithM attempt v w
  where
  attempt :: Term -> Sym -> Maybe (Maybe (Var, Sym))
  attempt (TVar var) sym  = return $ Just (var,sym)
  attempt (TSym sym) sym'
    | sym == sym' = return Nothing
    | otherwise   = Nothing
