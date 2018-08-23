{-# LANGUAGE DataKinds #-}

module Fixture.LinearAncestor
  ( program
  , edb
  ) where

import Protolude

import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation

import Util.Vector

parPred :: Predicate ('Succ ('Succ 'Zero)) 'ABase
parPred = Predicate PredABase "par" (SSucc (SSucc SZero)) Logical

ancPred :: Predicate ('Succ ('Succ 'Zero)) 'ABase
ancPred = Predicate PredABase "anc" (SSucc (SSucc SZero)) Logical

-- |Smart constructor for terms
tvar :: Text -> Term
tvar = TVar . Var

parLit :: Term -> Term -> Literal 'ABase
parLit t1 t2 = Literal LitABase Positive parPred (t1 ::: t2 ::: Nil)

ancLit :: Term -> Term -> Literal 'ABase
ancLit t1 t2 = Literal LitABase Positive ancPred (t1 ::: t2 ::: Nil)

program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (ancLit (tvar "X") (tvar "Z")) $ NE.fromList
      [ parLit (tvar "X") (tvar "Y"), ancLit (tvar "Y") (tvar "Z") ]
  , Clause ClABase (ancLit (tvar "X") (tvar "Y")) $ NE.fromList
      [ parLit (tvar "X") (tvar "Y") ]
  ]

edb :: Solution 'ABase
edb = fromList
  -- Parent
  [ relation parPred . T.fromList $ fmap Sym <$>
    [ "Laurent"     ::: "Mistral" ::: Nil
    , "Nilufer"     ::: "Mistral" ::: Nil
    , "Jean-Pierre" ::: "Laurent" ::: Nil
    , "Simone"      ::: "Laurent" ::: Nil
    , "Orhan"       ::: "Nilufer" ::: Nil
    , "Orhan"       ::: "Hulusi"  ::: Nil
    , "Nazli"       ::: "Emir"    ::: Nil
    , "Hulusi"      ::: "Emir"    ::: Nil
    ]
  -- Ancestor
  , relation ancPred . T.fromList $ fmap Sym <$>
    -- From the first clause
    [ "Laurent"     ::: "Mistral" ::: Nil
    , "Nilufer"     ::: "Mistral" ::: Nil
    , "Jean-Pierre" ::: "Laurent" ::: Nil
    , "Simone"      ::: "Laurent" ::: Nil
    , "Orhan"       ::: "Nilufer" ::: Nil
    , "Orhan"       ::: "Hulusi"  ::: Nil
    , "Nazli"       ::: "Emir"    ::: Nil
    , "Hulusi"      ::: "Emir"    ::: Nil
    -- From the second clause
    , "Orhan"       :::  "Mistral" ::: Nil
    , "Jean-Pierre" :::  "Mistral" ::: Nil
    , "Simone"      :::  "Mistral" ::: Nil
    , "Orhan"       :::  "Emir"    ::: Nil
    ]
  ]
