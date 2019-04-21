{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Exalog.WellModing
  ( WellModable(..)
  ) where

import Protolude hiding (head)

import qualified Data.Vector.Sized as V

import Language.Exalog.Adornment
import Language.Exalog.SrcLoc (span)
import Language.Exalog.Core
import Language.Exalog.Logger

class WellModable ast where
  checkWellModed :: ast -> Logger ()
  isWellModed    :: ast -> Bool

instance SpannableAnn (LiteralAnn ann)
    => WellModable (Program ('AAdornment ann)) where
  checkWellModed Program{..} = traverse_ checkWellModed clauses
  isWellModed    Program{..} = all isWellModed clauses

instance SpannableAnn (LiteralAnn ann)
    => WellModable (Clause ('AAdornment ann)) where
  checkWellModed Clause{..} = traverse_ checkWellModed body
  isWellModed    Clause{..} = all isWellModed body

instance SpannableAnn (LiteralAnn ann)
    => WellModable (Literal ('AAdornment ann)) where
  checkWellModed lit =
    when (isWellModed lit) $ scold (Just $ span lit) "Not well-moded."
  isWellModed lit@Literal{..}
    | Positive <- polarity = True
    | otherwise = (`all` zip (adornment lit) (V.toList terms)) $ \case
      (Free,TVar{}) -> False
      _             -> True
