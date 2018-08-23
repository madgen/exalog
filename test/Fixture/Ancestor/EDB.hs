{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.EDB (initEDB, finalEDB) where

import Protolude

import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation
import           Language.Exalog.Core

import Util.Vector

import Fixture.Ancestor.Common

parentTuples :: [ Vector ('Succ ('Succ 'Zero)) Text ]
parentTuples =
  [ "Laurent"     ::: "Mistral" ::: Nil
  , "Nilufer"     ::: "Mistral" ::: Nil
  , "Jean-Pierre" ::: "Laurent" ::: Nil
  , "Simone"      ::: "Laurent" ::: Nil
  , "Orhan"       ::: "Nilufer" ::: Nil
  , "Orhan"       ::: "Hulusi"  ::: Nil
  , "Nazli"       ::: "Emir"    ::: Nil
  , "Hulusi"      ::: "Emir"    ::: Nil
  ]

parentRel :: Relation 'ABase
parentRel = Relation parPred . T.fromList $ fmap Sym <$> parentTuples

ancestorTuples :: [ Vector ('Succ ('Succ 'Zero)) Text ]
ancestorTuples =
  -- From the first clause
  parentTuples ++
  -- From the second clause
  [ "Orhan"       ::: "Mistral" ::: Nil
  , "Jean-Pierre" ::: "Mistral" ::: Nil
  , "Simone"      ::: "Mistral" ::: Nil
  , "Orhan"       ::: "Emir"    ::: Nil
  ]

ancestorRel :: Relation 'ABase
ancestorRel = Relation ancPred . T.fromList $ fmap Sym <$> ancestorTuples

initEDB :: Solution 'ABase
initEDB = [ parentRel ]

finalEDB :: Solution 'ABase
finalEDB = fromList
  -- Parent
  [ parentRel
  -- Ancestor
  , ancestorRel
  ]
