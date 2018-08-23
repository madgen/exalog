{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.Common
  ( parPred, parLit
  , ancPred, ancLit
  ) where

import Language.Exalog.Core

import Util.Vector

parLit :: Term -> Term -> Literal 'ABase
parLit t1 t2 = Literal LitABase Positive parPred (t1 ::: t2 ::: Nil)

ancLit :: Term -> Term -> Literal 'ABase
ancLit t1 t2 = Literal LitABase Positive ancPred (t1 ::: t2 ::: Nil)

parPred :: Predicate ('Succ ('Succ 'Zero)) 'ABase
parPred = Predicate PredABase "par" (SSucc (SSucc SZero)) Logical

ancPred :: Predicate ('Succ ('Succ 'Zero)) 'ABase
ancPred = Predicate PredABase "anc" (SSucc (SSucc SZero)) Logical
