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

import           Data.Finite (getFinite)
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
  checkWellModedness Program{..} = traverse_ checkWellModedness (join $ map _unStratum _strata)
  isWellModed        Program{..} = all isWellModed $ join $ map _unStratum _strata

instance ( SpannableAnn (LiteralAnn ann)
         , Moded (Clause ('AAdornment ann))
         ) => WellModed (Clause ann) where
  checkWellModedness cl@Clause{..} = checkWellModability $ adornClause (allFree _head) cl
  isWellModed        cl@Clause{..} = isWellModable       $ adornClause (allFree _head) cl

allFree :: Literal ann -> [ Adornment ]
allFree Literal{_predicate = Predicate{..}} =
  replicate (fromIntegral . fromSing $ _arity) Free

class Moded ast where
  checkWellModability :: ast -> Logger ()
  isWellModable       :: ast -> Bool

instance SpannableAnn (LiteralAnn ann)
    => Moded (Program ('AAdornment ann)) where
  checkWellModability Program{..} = traverse_ checkWellModability $ join $ map _unStratum _strata
  isWellModable       Program{..} = all isWellModable $ join $ map _unStratum _strata

instance SpannableAnn (LiteralAnn ann) => Moded (Clause ('AAdornment ann)) where
  checkWellModability Clause{..} = traverse_ checkWellModability _body
  isWellModable       Clause{..} = all isWellModable _body

instance SpannableAnn (LiteralAnn ann)
    => Moded (Literal ('AAdornment ann)) where
  checkWellModability lit =
    unless (isWellModable lit) $ scold (Just $ span lit) "Not well-modable."
  isWellModable lit@Literal{..}
    | Positive <- _polarity = True
    | otherwise = (`all` zip (adornment lit) (V.toList _terms)) $ \case
      (Free, TVar{}) -> False
      _              -> True

fixModing :: (Program ('ARename 'ABase), R.Solution ('ARename 'ABase))
          -> Logger (Program 'ABase, R.Solution 'ABase)
fixModing =
  fixDataflow modingViolations
              "Not well-moded and cannot be repaired due to its dataflow."

modingViolations :: Monad m
                 => Clause ('ARename 'ABase)
                 -> RepairT m [ (FlowSink 'ABase, Var)  ]
modingViolations Clause{_body = body} = do
  flowGr <- getPositiveFlowGraph

  pure $ sconcat $ (<$> body) $ \lit@Literal{..} ->
    catMaybes . V.toList $ (`V.imap` _terms) $ \fin -> \case
      TVar var ->
        let ix = fromInteger $ getFinite fin
        in if _polarity == Negative && isPredPredicate flowGr lit ix
             then Just (FSinkLiteral lit ix, var)
             else Nothing
      _ -> Nothing
