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
import qualified Language.Exalog.KnowledgeBase.Class as KB
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import           Language.Exalog.Renamer ()
import           Language.Exalog.Logger
import           Language.Exalog.SrcLoc

data RepairResult kb =
    DeadDataPath
  | NotFixable
  | Guard (Literal 'ABase) [ Clause 'ABase ] (kb 'ABase)

type Repair = RepairT Logger

fixDataflow :: KB.Knowledgeable kb 'ABase
            => KB.Knowledgeable kb ('ARename 'ABase)
            => Monoid (kb 'ABase)
            => (Clause ('ARename 'ABase) -> Repair [ (FlowSink 'ABase, Var) ])
            -> Text
            -> (Program ('ARename 'ABase), kb ('ARename 'ABase))
            -> Logger (Program 'ABase, kb 'ABase)
fixDataflow violationFinder errMsg (pr@Program{..}, sol)
  | [ Stratum clauses ] <- _strata = runRepairT pr $ do
    (originalClauses, guardClausess, guardSols) <- unzip3 <$>
      traverse (fixDataflowClause violationFinder errMsg) clauses

    pure ( Program
            { _annotation = peelA _annotation
            , _strata     = [ Stratum $ originalClauses <> join guardClausess ]
            , _queries    = (PredicateBox . peel $$) <$> _queries
            , ..}
         , mconcat (KB.atEach (\(KB.Knowledge pred syms) -> KB.Knowledge (peel pred) syms) sol : guardSols)
         )
  | otherwise = scream NoSpan
    "Dataflow repair can only be performed prior to stratification."

fixDataflowClause :: Monoid (kb 'ABase)
                  => KB.Knowledgeable kb 'ABase
                  => (Clause ('ARename 'ABase) -> Repair [ (FlowSink 'ABase, Var) ])
                  -> Text
                  -> Clause ('ARename 'ABase)
                  -> Repair (Clause 'ABase, [ Clause 'ABase ], kb 'ABase)
fixDataflowClause violationFinder errMsg cl@Clause{..} = do
  violations <- violationFinder cl
  repairResults <- traverse (uncurry (attemptFix $ span _head)) violations

  (guardLits, guardClausess, guardSols) <-
    fmap (unzip3 . catMaybes) $ forM repairResults $ \case
      Guard gLit gCls gSol -> pure $ Just (gLit, gCls, gSol)
      DeadDataPath         -> pure Nothing
      NotFixable           -> lift $ lift $ scold (span _head) errMsg

  pure ( Clause
          { _annotation = peelA _annotation
          , _head       = peel _head
          , _body       = foldr' NE.cons (peel <$> _body) guardLits
          }
       , join guardClausess
       , mconcat guardSols
       )

attemptFix :: Monad m
           => Monoid (kb 'ABase)
           => KB.Knowledgeable kb 'ABase
           => SrcSpan
           -> FlowSink 'ABase
           -> Var
           -> RepairT m (RepairResult kb)
attemptFix sp flowSink var = do
  flowGr <- getPositiveFlowGraph

  case nearestCoveringPositives flowGr flowSink of
    Just flowSources -> mkGuard sp flowSources var
    Nothing          -> pure DeadDataPath

mkGuard :: Monad m
        => Monoid (kb 'ABase)
        => KB.Knowledgeable kb 'ABase
        => SrcSpan
        -> [ FlowSource 'ABase ]
        -> Var
        -> RepairT m (RepairResult kb)
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

mkGuardFact :: Identifiable (PredicateAnn ann) id
            => KB.Knowledgeable kb ann
            => Predicate 1 ann -> Sym -> kb ann
mkGuardFact guardPred sym = KB.singleton $
  KB.Knowledge guardPred (V.singleton sym)

mkGuardClause :: SrcSpan -> Literal 'ABase -> Body 'ABase -> Clause 'ABase
mkGuardClause sp head body = Clause
  { _annotation = ClABase sp
  , _head       = head
  , _body       = body
  }

mkGuardBody :: SrcSpan -> Literal ('ARename 'ABase) -> Var -> Int -> Body 'ABase
mkGuardBody sp Literal{_predicate = guardPred@Predicate{..}} var ix = do
  let ts = replicate (fromIntegral . fromSing $ _arity) TWild

  V.withSizedList ts $ \(vts :: V.Vector n Term) ->
    case (sing :: SNat n) %~ _arity of
      Proved Refl -> (NE.:| []) $ Literal
         { _annotation = LitABase sp
         , _predicate  = peel guardPred
         , _terms      = V.unsafeUpd vts [(ix,TVar var)]
         , _polarity   = Positive
         }
      _ -> panic "Argument vector generation failed."

mkGuardPredicate :: PredicateSymbol -> SrcSpan -> Predicate 1 'ABase
mkGuardPredicate predSym sp = Predicate
  { _annotation = PredABase sp
  , _predSym    = predSym
  , _nature     = Logical
  , _arity      = sing :: SNat 1
  }

mkGuardLiteral :: Predicate 1 'ABase -> SrcSpan -> Term -> Literal 'ABase
mkGuardLiteral pred sp term = Literal
  { _annotation = LitABase sp
  , _predicate  = pred
  , _terms      = V.singleton term
  , _polarity   = Positive
  }

type RepairEnv = PositiveFlowGr 'ABase
type RepairT m = ReaderT RepairEnv (FreshT m)

runRepairT :: Monad m => Program ('ARename 'ABase) -> RepairT m a -> m a
runRepairT pr = runFreshT (Just "guard") reserved . (`runReaderT` flowGr)
  where
  flowGr = analysePositiveFlow pr
  reserved  = ((\Predicate{_predSym = PredicateSymbol txt} -> txt) $$)
          <$> predicates pr

getPositiveFlowGraph :: Monad m => RepairT m (PositiveFlowGr 'ABase)
getPositiveFlowGraph = ask

getFreshPredSym :: Monad m => RepairT m PredicateSymbol
getFreshPredSym = lift $ PredicateSymbol <$> fresh
