{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.EDB (initEDB, finalEDB) where

import Protolude hiding (Set)

import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V

import qualified Language.Exalog.KnowledgeBase.Class as KB
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.Core

import Fixture.Ancestor.Common
import Fixture.Util

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

parentKB :: Set 'ABase
parentKB = KB.fromList $ mkKnowledge parPred . fmap symbol <$> parentTuples

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

ancestorKB :: Set 'ABase
ancestorKB = KB.fromList $ mkKnowledge ancPred . fmap symbol <$> ancestorTuples

initEDB :: Set 'ABase
initEDB = parentKB

finalEDB :: Set 'ABase
finalEDB = parentKB <> ancestorKB
