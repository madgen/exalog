{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.EDB (initEDB, finalEDB) where

import Protolude

import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation
import           Language.Exalog.Core

import Fixture.Ancestor.Common

parentTuples :: [ V.Vector 2 Text ]
parentTuples = fromJust . V.fromList <$>
  [ [ "Laurent"     , "Mistral" ]
  , [ "Nilufer"     , "Mistral" ]
  , [ "Jean-Pierre" , "Laurent" ]
  , [ "Simone"      , "Laurent" ]
  , [ "Orhan"       , "Nilufer" ]
  , [ "Orhan"       , "Hulusi"  ]
  , [ "Nazli"       , "Emir"    ]
  , [ "Hulusi"      , "Emir"    ]
  , [ "Omer"        , "Orhan"   ]
  ]

parentRel :: Relation 'ABase
parentRel = Relation parPred . T.fromList $ fmap Sym <$> parentTuples

ancestorTuples :: [ V.Vector 2 Text ]
ancestorTuples =
  -- From the first clause
  (parentTuples ++) $ fromJust . V.fromList <$>
  -- From the second clause
  [ [ "Orhan"       , "Mistral" ]
  , [ "Jean-Pierre" , "Mistral" ]
  , [ "Simone"      , "Mistral" ]
  , [ "Orhan"       , "Emir"    ]
  , [ "Omer"        , "Nilufer" ]
  , [ "Omer"        , "Hulusi"  ]
  , [ "Omer"        , "Mistral" ]
  , [ "Omer"        , "Emir"    ]
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
