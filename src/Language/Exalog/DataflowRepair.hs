{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.DataflowRepair
  ( fixDataflow
  , RepairT
  , getPositiveFlowGraph
  ) where

import Protolude hiding (sym, head, pred)

import           Data.List (unzip3)
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
import           Language.Exalog.Logger
import           Language.Exalog.SrcLoc
import qualified Language.Exalog.Tuples as T

data RepairResult =
    DeadDataPath
  | NotFixable
  | Guard (Literal 'ABase) [ Clause 'ABase ] (R.Solution 'ABase)

type Repair = RepairT Logger

fixDataflow :: (Clause ('ARename 'ABase) -> Repair [ (FlowSink 'ABase, Var) ])
            -> Text
            -> (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
            -> Logger (Program 'ABase, R.Solution 'ABase)
fixDataflow violationFinder errMsg (pr@Program{..}, sol)
  | [ clauses ] <- strata = runRepairT pr $ do
    (originalClauses, guardClausess, guardSols) <- unzip3 <$>
      traverse (fixDataflowClause violationFinder errMsg) clauses

    pure ( Program
            { annotation = peelA annotation
            , strata     = [ originalClauses <> join guardClausess ]
            , queryPreds = (PredicateBox . peel $$) <$> queryPreds
            , ..}
         , mconcat (R.rename peel sol : guardSols)
         )
  | otherwise = scream Nothing
    "Dataflow repair can only be performed prior to stratification."

fixDataflowClause :: (Clause ('ARename 'ABase) -> Repair [ (FlowSink 'ABase, Var) ])
                  -> Text
                  -> Clause ('ARename 'ABase)
                  -> Repair (Clause 'ABase, [ Clause 'ABase ], R.Solution 'ABase)
fixDataflowClause violationFinder errMsg cl@Clause{..} = do
  violations <- violationFinder cl
  repairResults <- traverse (uncurry (attemptFix $ span head)) violations

  (guardLits, guardClausess, guardSols) <-
    fmap (unzip3 . catMaybes) $ forM repairResults $ \case
      Guard gLit gCls gSol -> pure $ Just (gLit, gCls, gSol)
      DeadDataPath         -> pure Nothing
      NotFixable           -> lift $ lift $ scold (Just $ span head) errMsg

  pure ( Clause
          { annotation = peelA annotation
          , head       = peel head
          , body       = foldr' NE.cons (peel <$> body) guardLits
          , ..}
       , join guardClausess
       , mconcat guardSols
       )

attemptFix :: Monad m
           => SrcSpan -> FlowSink 'ABase
           -> Var
           -> RepairT m RepairResult
attemptFix sp flowSink var = do
  flowGr <- getPositiveFlowGraph

  case nearestCoveringPositives flowGr flowSink of
    Just flowSources -> mkGuard sp flowSources var
    Nothing          -> pure DeadDataPath

mkGuard :: Monad m
        => SrcSpan
        -> [ FlowSource 'ABase ]
        -> Var
        -> RepairT m RepairResult
mkGuard sp flowSources var = do
  guardSym <- getFreshPredSym

  let guardPred = mkGuardPredicate guardSym sp
  let guardLit  = mkGuardLiteral guardPred sp (TVar var)

  let mGuard = do
        eClausesFacts <- forM flowSources $ \case
          FSourceLiteral lit ix -> Just $ Left $
            mkGuardClause sp guardLit (mkGuardBody sp lit var ix)

          FSourceConstant constant ->
            case constant of
              CSym sym -> Just $ Right $ mkGuardFact guardPred sym
              CWild    -> Nothing

        pure $ partitionEithers eClausesFacts

  pure $ maybe NotFixable (uncurry (Guard guardLit) . second mconcat) mGuard

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
