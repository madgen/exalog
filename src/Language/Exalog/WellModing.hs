{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.WellModing
  ( WellModed(..)
  , Moded(..)
  , fixModing
  ) where

import Protolude hiding (head)

import           Data.List (unzip3)
import           Data.Finite (getFinite)
import qualified Data.List.NonEmpty as NE
import           Data.Singletons (fromSing)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Adornment
import           Language.Exalog.Core
import           Language.Exalog.Dataflow
import           Language.Exalog.DataflowRepair
import           Language.Exalog.Logger
import qualified Language.Exalog.Relation as R
import           Language.Exalog.SrcLoc (span)

class WellModed ast where
  checkWellModedness :: ast -> Logger ()
  isWellModed        :: ast -> Bool

instance ( SpannableAnn (LiteralAnn ann)
         , Moded (Program ('AAdornment ann))
         ) => WellModed (Program ann) where
  checkWellModedness Program{..} = traverse_ checkWellModedness clauses
  isWellModed        Program{..} = all isWellModed clauses

instance ( SpannableAnn (LiteralAnn ann)
         , Moded (Clause ('AAdornment ann))
         ) => WellModed (Clause ann) where
  checkWellModedness cl@Clause{..} = checkWellModability $ adornClause (allFree head) cl
  isWellModed        cl@Clause{..} = isWellModable       $ adornClause (allFree head) cl

allFree :: Literal ann -> [ Adornment ]
allFree Literal{predicate = Predicate{..}} =
  replicate (fromIntegral . fromSing $ arity) Free

class Moded ast where
  checkWellModability :: ast -> Logger ()
  isWellModable       :: ast -> Bool

instance SpannableAnn (LiteralAnn ann)
    => Moded (Program ('AAdornment ann)) where
  checkWellModability Program{..} = traverse_ checkWellModability clauses
  isWellModable       Program{..} = all isWellModable clauses

instance SpannableAnn (LiteralAnn ann) => Moded (Clause ('AAdornment ann)) where
  checkWellModability Clause{..} = traverse_ checkWellModability body
  isWellModable       Clause{..} = all isWellModable body

instance SpannableAnn (LiteralAnn ann)
    => Moded (Literal ('AAdornment ann)) where
  checkWellModability lit =
    unless (isWellModable lit) $ scold (Just $ span lit) "Not well-modable."
  isWellModable lit@Literal{..}
    | Positive <- polarity = True
    | otherwise = (`all` zip (adornment lit) (V.toList terms)) $ \case
      (Free, TVar{}) -> False
      _              -> True

fixModing :: (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
          -> Logger (Program 'ABase, R.Solution 'ABase)
fixModing (pr@Program{..}, sol) = runRepairT pr $ do
  (originalClauses, guardClausess, guardSols) <- unzip3 <$>
    traverse fixModingClause clauses

  pure ( Program
          { annotation = peelA annotation
          , clauses    = originalClauses <> join guardClausess
          , queryPreds = (PredicateBox . peel $$) <$> queryPreds
          , ..}
       , mconcat (R.rename peel sol : guardSols)
       )

fixModingClause :: Clause ('ARename 'ABase)
                -> RepairT Logger
                    (Clause 'ABase, [ Clause 'ABase ], R.Solution 'ABase)
fixModingClause cl@Clause{..} = do
  violations <- modingViolations cl
  repairResults <- traverse (uncurry (attemptFix $ span head)) violations

  (guardLits, guardClausess, guardSols) <-
    fmap (unzip3 . catMaybes) $ forM repairResults $ \case
      Guard gLit gCls gSol -> pure $ Just (gLit, gCls, gSol)
      DeadDataPath         -> pure Nothing
      NotFixable           -> lift $ lift $ scold (Just $ span head)
        "Not range-restricted and cannot be repaired due to its dataflow."

  pure ( Clause
          { annotation = peelA annotation
          , head       = peel head
          , body       = foldr' NE.cons (peel <$> body) guardLits
          , ..}
       , join guardClausess
       , mconcat guardSols
       )

modingViolations :: Monad m
                 => Clause ('ARename 'ABase)
                 -> RepairT m [ (FlowSink 'ABase, Var)  ]
modingViolations Clause{body = body} = do
  flowGr <- getPositiveFlowGraph

  pure $ sconcat $ (<$> body) $ \lit@Literal{..} ->
    catMaybes . V.toList $ (`V.imap` terms) $ \fin term ->
      case term of
        TVar var ->
          let ix = fromInteger $ getFinite fin
          in if isPredPredicate flowGr lit ix
               then Just (FSinkLiteral lit ix, var)
               else Nothing
        _ -> Nothing
