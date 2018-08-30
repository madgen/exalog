{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Fixture.Ancestor.Common
  ( parPred, parLit
  , ancPred, ancLit
  ) where

import Protolude

import           Data.Maybe (fromJust)
import           Data.Singletons.TypeLits
import qualified Data.Vector.Sized as V

import Language.Exalog.Core

parLit :: Term -> Term -> Literal 'ABase
parLit t1 t2 = Literal LitABase Positive parPred $ fromJust $ V.fromList [ t1, t2 ]

ancLit :: Term -> Term -> Literal 'ABase
ancLit t1 t2 = Literal LitABase Positive ancPred $ fromJust $ V.fromList [ t1, t2 ]

parPred :: Predicate 2 'ABase
parPred = Predicate PredABase "par" (SNat @2) Logical

ancPred :: Predicate 2 'ABase
ancPred = Predicate PredABase "anc" (SNat @2) Logical
