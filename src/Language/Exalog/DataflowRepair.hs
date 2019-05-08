{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.DataflowRepair
  ( RepairT
  , runRepairT
  , attemptFix
  ) where

import Protolude hiding (sym, head, pred)

import           Data.Singletons (sing, fromSing)
import           Data.Singletons.TypeLits (SNat)
import           Data.Singletons.Decide (Decision(..), (%~))
import qualified Data.Vector.Sized as V
import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import           Language.Exalog.Dataflow
import           Language.Exalog.Fresh
import           Language.Exalog.Renamer ()
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SrcLoc
import qualified Language.Exalog.Tuples as T

attemptFix :: Monad m
           => SrcSpan -> FlowSink 'ABase
           -> Var
           -> RepairT m
               (Maybe ( Literal 'ABase
                      , [ Clause 'ABase ]
                      , R.Solution 'ABase
                      ))
attemptFix sp flowSink var = do
  flowGr <- getPositiveFlowGraph

  case nearestCoveringPositives flowGr flowSink of
    Just flowSources -> mkGuard sp flowSources var
    Nothing          -> pure Nothing

mkGuard :: Monad m
        => SrcSpan
        -> [ FlowSource 'ABase ]
        -> Var
        -> RepairT m
           (Maybe ( Literal 'ABase
                  , [ Clause 'ABase ]
                  , R.Solution 'ABase
                  ))
mkGuard sp flowSources var = do
  guardSym <- getFreshPredSym

  let guardPred = mkGuardPredicate guardSym sp
  let guardLit  = mkGuardLiteral guardPred sp (TVar var)

  pure $ do
    eClausesFacts <- forM flowSources $ \case
      FSourceLiteral lit ix -> pure $ Left $
        mkGuardClause sp guardLit (mkGuardBody sp lit var ix)

      FSourceConstant constant ->
        case constant of
          CSym sym -> pure $ Right $ mkGuardFact guardPred sym
          CWild    -> Nothing

    let (clauses, sols) = partitionEithers eClausesFacts

    pure (guardLit, clauses, mconcat sols)

mkGuardFact :: IdentifiableAnn (PredicateAnn ann) a => Ord a
            => Predicate 1 ann -> Sym -> R.Solution ann
mkGuardFact guardPred sym = R.fromList
  [ R.Relation guardPred (T.fromList [ V.singleton sym ]) ]

mkGuardClause :: SrcSpan -> Literal 'ABase -> Body 'ABase -> Clause 'ABase
mkGuardClause sp head body = Clause
  { annotation = ClABase sp
  , head       = head
  , body       = body
  }

mkGuardBody :: SrcSpan -> Literal ('ARename 'ABase) -> Var -> Int -> Body 'ABase
mkGuardBody sp Literal{predicate = guardPred@Predicate{..}} var ix = do
  let ts = replicate (fromIntegral . fromSing $ arity) TWild

  V.withSizedList ts $ \(vts :: V.Vector n Term) ->
    case (sing :: SNat n) %~ arity of
      Proved Refl -> (NE.:| []) $ Literal
         { annotation = LitABase sp
         , predicate  = peel guardPred
         , terms      = V.unsafeUpd vts [(ix,TVar var)]
         , polarity   = Positive
         }
      _ -> panic "Argument vector generation failed."

mkGuardPredicate :: PredicateSymbol -> SrcSpan -> Predicate 1 'ABase
mkGuardPredicate pSym sp = Predicate
  { annotation = PredABase sp
  , fxSym      = pSym
  , nature     = Logical
  , arity      = sing :: SNat 1
  }

mkGuardLiteral :: Predicate 1 'ABase -> SrcSpan -> Term -> Literal 'ABase
mkGuardLiteral pred sp term = Literal
  { annotation = LitABase sp
  , predicate  = pred
  , terms      = V.singleton term
  , polarity   = Positive
  }

type RepairEnv = PositiveFlowGr 'ABase
type RepairT m = ReaderT RepairEnv (FreshT m)

runRepairT :: Monad m => Program ('ARename 'ABase) -> RepairT m a -> m a
runRepairT pr = runFreshT (Just "guard") reserved
             . (`runReaderT` flowGr)
  where
  flowGr = analysePositiveFlow pr
  reserved  = ((\Predicate{fxSym = PredicateSymbol txt} -> txt) $$)
          <$> predicates pr

getPositiveFlowGraph :: Monad m => RepairT m (PositiveFlowGr 'ABase)
getPositiveFlowGraph = ask

getFreshPredSym :: Monad m => RepairT m PredicateSymbol
getFreshPredSym = lift $ PredicateSymbol <$> fresh
