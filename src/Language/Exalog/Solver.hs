{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.Solver
  ( solve
  , compute
  , evalSolver
  ) where

import Protolude

import Data.List (partition)

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Delta ( mkDeltaStratum
                                       , mkDeltaSolution
                                       , cleanDeltaSolution
                                       )
import           Language.Exalog.SemiNaive ( SemiNaive
                                           , evalSemiNaiveT
                                           , semiNaive
                                           , evalClauses
                                           )
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Class as KB

data SolverSt kb ann = SolverSt
    { _program :: Program ann
    , _initEDB :: kb ann
    }

type Solver kb ann = StateT (SolverSt kb ann) (SemiNaive (kb ann))

solve :: SpannableAST a
      => Identifiable (PredicateAnn a) b
      => KB.Knowledgeable kb a => KB.Knowledgeable kb ('ADelta a)
      => Monoid (kb a) => Semigroup (kb ('ADelta a))
      => Program a -> kb a -> Logger (kb a)
solve = evalSolver compute

evalSolver :: Identifiable (PredicateAnn ann) b
           => KB.Knowledgeable kb ann
           => Monoid (kb ann)
           => Solver kb ann a -> Program ann -> kb ann -> Logger a
evalSolver action pr sol = evalSemiNaiveT (evalStateT action (SolverSt pr sol)) mempty

compute :: SpannableAST a
        => Identifiable (PredicateAnn a) b
        => KB.Knowledgeable kb a => KB.Knowledgeable kb ('ADelta a)
        => Semigroup (kb a) => Semigroup (kb ('ADelta a))
        => Solver kb a (kb a)
compute = do
  pr      <- _program <$> get
  initEDB <- _initEDB <$> get
  let strat = _strata pr

  finalEDB <- lift $
    foldM (\edb -> local (const edb) . evalStratum) initEDB strat

  -- Filter out non-query solutions
  let qPreds = _queries pr
  pure $ KB.filter (\(KB.Knowledge p _) -> PredicateBox p `elem` qPreds) finalEDB

evalStratum :: forall a b kb
             . SpannableAST a
            => Identifiable (PredicateAnn a) b
            => KB.Knowledgeable kb a => KB.Knowledgeable kb ('ADelta a)
            => Semigroup (kb a) => Semigroup (kb ('ADelta a))
            => Stratum a -> SemiNaive (kb a) (kb a)
evalStratum stratum@(Stratum cls) = do
  simpleEDB <-
    if null simpleClauses
      then ask
      else evalClauses simpleClauses

  local (const simpleEDB) $
    if null (_unStratum complexStratum)
      then ask
      else cleanDeltaSolution <$>
        (withDifferentEnvironment envMap
         . semiNaive
         $ deltaStratum)
  where
  (simpleClauses, complexStratum) = second Stratum $ partitionBySimplicity cls
  deltaStratum                    = mkDeltaStratum complexStratum

  envMap = mkDeltaSolution (intentionals complexStratum)

  intentionalPreds = intentionals stratum

  partitionBySimplicity :: [ Clause a ] -> ([ Clause a ], [ Clause a ])
  partitionBySimplicity =
    partition (all ((`notElem` intentionalPreds) . predicateBox) . _body)

withDifferentEnvironment :: Monad m
                         => (r -> s) -> ReaderT s m a -> ReaderT r m a
withDifferentEnvironment envMap (ReaderT f) =
  ReaderT $ f . envMap
