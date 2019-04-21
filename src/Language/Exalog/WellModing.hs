{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.WellModing
  ( WellModed(..)
  , Moded(..)
  ) where

import Protolude hiding (head)

import           Data.Singletons (fromSing)
import qualified Data.Vector.Sized as V

import Language.Exalog.Adornment
import Language.Exalog.SrcLoc (span)
import Language.Exalog.Core
import Language.Exalog.Logger

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
    when (isWellModable lit) $ scold (Just $ span lit) "Not well-modable."
  isWellModable lit@Literal{..}
    | Positive <- polarity = True
    | otherwise = (`all` zip (adornment lit) (V.toList terms)) $ \case
      (Free,TVar{}) -> False
      _             -> True
