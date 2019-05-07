{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.RangeRestriction
  ( RangeRestriction(..)
  , fixRangeRestriction
  ) where

import Protolude hiding (diff, head, pred, sym)

import Control.Arrow ((&&&))

import           Data.List ((\\), unzip3)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (sing, fromSing)
import           Data.Singletons.Decide (Decision(..), (%~))
import           Data.Singletons.TypeLits (SNat)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Logger
import           Language.Exalog.Dataflow
import           Language.Exalog.Fresh
import           Language.Exalog.Renamer ()
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.SrcLoc (Spannable(..),SrcSpan)

-- |Checks if all variables in the head appear in the bodies of the
-- clauses.
--
-- This allows domain independence (changing the underlying types in a way
-- compatible with the data in the database/EDB) does not cause a change in
-- the query results.
--
-- It also contributes to finiteness of results (allowing tabulation of all
-- ground facts) by preventing concluding facts such as $p(X,X)$.
--
-- A final benefit is facilitating data provenance. This restriction does
-- not solely ensure but contribute to the fact that all ground terms we
-- use come from somewhere in the original EDB.
class RangeRestriction ast where
  checkRangeRestriction      :: ast -> Logger ()
  isRangeRestricted          :: ast -> Bool

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Program ann) where
  checkRangeRestriction Program{..} = traverse_ checkRangeRestriction clauses
  isRangeRestricted Program{..} = all isRangeRestricted clauses

instance SpannableAnn (ClauseAnn ann) => RangeRestriction (Clause ann) where
  checkRangeRestriction cl =
    unless (isRangeRestricted cl) $
      scold (Just $ span cl) "Range restriction is violated."

  isRangeRestricted Clause{..} =
    null $ variables head \\ mconcat (variables <$> NE.toList body)

fixRangeRestriction :: (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
                    -> Logger (Program 'ABase, R.Solution 'ABase)
fixRangeRestriction (pr@Program{..}, sol) = runRepairT pr $ do
  (originalClauses, guardClausess, guardSols) <- unzip3 <$>
    traverse fixRangeRestrictionClause clauses

  pure ( Program
          { annotation = peelA annotation
          , clauses    = originalClauses <> join guardClausess
          , queryPreds = (PredicateBox . peel $$) <$> queryPreds
          , ..}
       , mconcat (R.rename peel sol : guardSols)
       )

fixRangeRestrictionClause :: Clause ('ARename 'ABase)
                          -> RepairT Logger
                             ( Clause 'ABase
                             , [ Clause 'ABase ]
                             , R.Solution 'ABase
                             )
fixRangeRestrictionClause cl@Clause{..} = do
  mGuard <- sequence <$> traverse (uncurry (mkGuard $ span head)) violations

  case unzip3 <$> mGuard of
    Just (guardLits, guardClausess, guardSols) ->
      pure ( Clause
              { annotation = peelA annotation
              , head       = peel head
              , body       = foldr' NE.cons (peel <$> body) guardLits
              , ..}
           , join guardClausess
           , mconcat guardSols
           )
    Nothing -> lift $ lift $ scold (Just $ span head)
      "Not range-restricted and cannot be repaired due to its dataflow."
  where
  violations = rangeRestrictionViolations cl

mkGuard :: Monad m
        => SrcSpan
        -> FlowSink 'ABase
        -> Var
        -> RepairT m
           (Maybe ( Literal 'ABase
                  , [ Clause 'ABase ]
                  , R.Solution 'ABase
                  ))
mkGuard sp flowSink var = do
  flowGr <- getPositiveFlowGraph

  guardSym <- getFreshPredSym

  let guardPred = mkGuardPredicate guardSym sp
  let guardLit  = mkGuardLiteral guardPred sp (TVar var)

  pure $ do
    coveringPositives <- nearestCoveringPositives flowGr flowSink

    eClausesFacts <- forM (NE.toList coveringPositives) $ \case
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

rangeRestrictionViolations :: Clause ('ARename ann) -> [ (FlowSink ann, Var) ]
rangeRestrictionViolations Clause{..} = map (genSink . fst &&& snd)
                                      . filter (isRestriction . snd)
                                      . zip [0..]
                                      $ variables head
  where
  isRestriction var = var `notElem` bodyVariables
  genSink = FSinkPredicate (predicateBox head)
  bodyVariables = mconcat $ variables <$> NE.toList body

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
