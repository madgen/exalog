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

import qualified Data.Bimap as BM
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
import           Language.Exalog.Renamer
import qualified Language.Exalog.Relation as R
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.SrcLoc (Spannable(..),SrcSpan)
import           Language.Exalog.Pretty (pp)

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

fixRangeRestriction :: (Program 'ABase, R.Solution 'ABase)
                    -> Logger (Program 'ABase, R.Solution 'ABase)
fixRangeRestriction (pr@Program{..}, sol) = do
  renamedPr@Program{clauses = renamedClauses} <- rename pr

  runRepair renamedPr $ do

    (originalClauses, guardClausess, guardSols) <- unzip3 <$>
      traverse fixRangeRestrictionClause renamedClauses

    pure ( Program{clauses = originalClauses <> join guardClausess,..}
         , mconcat (sol : guardSols)
         )

fixRangeRestrictionClause :: Clause ('ARename 'ABase)
                          -> Repair ( Clause 'ABase
                                    , [ Clause 'ABase ]
                                    , R.Solution 'ABase
                                    )
fixRangeRestrictionClause cl@Clause{..} = do
  (guardLits, guardClausess, guardSols) <-
    unzip3 <$> traverse (uncurry (mkGuard $ span head)) violations

  pure ( Clause
          { annotation = peelA annotation
          , head       = peel head
          , body       = foldr' NE.cons (peel <$> body) guardLits
          , ..}
       , join guardClausess
       , mconcat guardSols
       )
  where
  violations = rangeRestrictionViolations cl

mkGuard :: SrcSpan
        -> FlowSink
        -> Var
        -> Repair (Literal 'ABase, [ Clause 'ABase ], R.Solution 'ABase)
mkGuard sp flowSink var = do
  flowGr <- getPositiveFlowGraph

  guardSym <- getFreshPredSym

  let guardPred = mkGuardPredicate guardSym sp
  let guardLit  = mkGuardLiteral guardPred sp (TVar var)

  case nearestCoveringPositives flowGr flowSink of
    Just flowSources -> do
      (clauses, sols) <- fmap (partitionEithers . NE.toList)
                       $ (`traverse` flowSources) $ \case
        FSourceLiteral litID ix -> do
          guardBody <- mkGuardBody sp litID var ix
          pure $ Left $ Clause
            { annotation = ClABase sp
            , head       = guardLit
            , body       = guardBody
            }

        FSourceConstant constant ->
          case constant of
            CSym sym -> pure . Right $ R.fromList
              [ R.Relation guardPred (T.fromList [ V.singleton sym ]) ]
            CWild -> lift $ lift $ scold (Just sp)
              "Range restriction is violated and it cannot be repaired due to dataflow."

      pure ( guardLit
           , clauses
           , mconcat sols
           )
    Nothing -> lift $ lift $ scold (Just sp)
      "Range restriction is violated and it cannot be repaired due to dataflow."

mkGuardBody :: SrcSpan -> LiteralID -> Var -> Int -> Repair (Body 'ABase)
mkGuardBody sp litID var ix = do
  PredicateBox guardPred@Predicate{..} <- findGuardPred litID

  let ts = replicate (fromIntegral . fromSing $ arity) TWild
  let ts' = take ix ts ++ TVar var : drop (ix + 1) ts

  V.withSizedList ts' $ \(vts :: V.Vector n Term) ->
    case (sing :: SNat n) %~ arity of
      Proved Refl -> pure $ (NE.:| []) $ Literal
         { annotation = LitABase sp
         , predicate  = peel guardPred
         , terms      = vts
         , polarity   = Positive
         }
      _ -> lift $ lift $ scream (Just sp)
        "Argument vector generation failed."

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

findGuardPred :: LiteralID -> Repair (PredicateBox ('ARename 'ABase))
findGuardPred litID@(LiteralID id) = do
  literalMap <- getLiteralMap

  case litID `BM.lookupR` literalMap of
    Just lit -> pure $ predicateBox lit
    Nothing  -> lift $ lift $ scream Nothing $
      "The literal id " <> show id <> " is not inside the literal map."

rangeRestrictionViolations :: Clause ('ARename ann) -> [ (FlowSink, Var) ]
rangeRestrictionViolations Clause{..} = map (genSink . fst &&& snd)
                                      . filter (isRestriction . snd)
                                      . zip [0..]
                                      $ variables head
  where
  isRestriction var = var `notElem` bodyVariables
  genSink = FSinkPredicate (predicateID head)
  bodyVariables = mconcat $ variables <$> NE.toList body

type LiteralMap = BM.Bimap (Literal ('ARename 'ABase)) LiteralID
type RepairEnv = (LiteralMap, PositiveFlowGr)
type Repair = ReaderT RepairEnv (FreshT Logger)

runRepair :: Program ('ARename 'ABase) -> Repair a -> Logger a
runRepair pr = runFreshT (Just "guard") reserved
             . (`runReaderT` (literalMap, flowGr))
  where
  flowGr = analysePositiveFlow pr
  reserved  = ((\Predicate{fxSym = PredicateSymbol txt} -> txt) $$)
          <$> predicates pr
  literalMap = mkLiteralMap pr

getLiteralMap :: Repair LiteralMap
getLiteralMap = fst <$> ask

getPositiveFlowGraph :: Repair PositiveFlowGr
getPositiveFlowGraph = snd <$> ask

getFreshPredSym :: Repair PredicateSymbol
getFreshPredSym = lift $ PredicateSymbol <$> fresh
