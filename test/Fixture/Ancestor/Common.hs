{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.Common
  ( parPred, anc
  , ancPred, par
  ) where

import Protolude

import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import Language.Exalog.Core
import Language.Exalog.SrcLoc

import Fixture.Util

anc :: Term -> Term -> Literal 'ABase
anc t t' = lit ancPred $ fromJust $ V.fromList [ t, t' ]

par :: Term -> Term -> Literal 'ABase
par t t' = lit parPred $ fromJust $ V.fromList [ t, t' ]

parPred :: Predicate 2 'ABase
parPred = Predicate (PredABase dummySpan) "par" SNat Logical

ancPred :: Predicate 2 'ABase
ancPred = Predicate (PredABase dummySpan) "anc" SNat Logical
