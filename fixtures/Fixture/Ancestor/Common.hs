{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.Common
  ( parPred, anc
  , ancPred, par
  , ancProv, parProv
  , parPredProv, ancPredProv
  ) where

import Protolude

import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import Language.Exalog.Core
import Language.Exalog.SrcLoc
import Language.Exalog.Provenance

import Fixture.Util

anc :: Term -> Term -> Literal 'ABase
anc t t' = lit ancPred $ fromJust $ V.fromList [ t, t' ]

par :: Term -> Term -> Literal 'ABase
par t t' = lit parPred $ fromJust $ V.fromList [ t, t' ]

parPred :: Predicate 2 'ABase
parPred = Predicate (PredABase NoSpan) "par" SNat Logical

ancPred :: Predicate 2 'ABase
ancPred = Predicate (PredABase NoSpan) "anc" SNat Logical

-- Decorated with provenance
litProv :: Predicate n ('AProvenance 'ABase) -> V.Vector n Term -> Literal ('AProvenance 'ABase)
litProv = Literal (LitAProvenance (LitABase NoSpan)) Positive

ancProv :: Term -> Term -> Literal ('AProvenance 'ABase)
ancProv t t' = litProv ancPredProv $ fromJust $ V.fromList [ t, t' ]

parProv :: Term -> Term -> Literal ('AProvenance 'ABase)
parProv t t' = litProv parPredProv $ fromJust $ V.fromList [ t, t' ]

parPredProv :: Predicate 2 ('AProvenance 'ABase)
parPredProv = Predicate (PredAProvenance (PredABase NoSpan)) "par" SNat Logical

ancPredProv :: Predicate 2 ('AProvenance 'ABase)
ancPredProv = Predicate (PredAProvenance (PredABase NoSpan)) "anc" SNat Logical