{-# LANGUAGE DataKinds #-}

module Fixture.Ancestor.EDB
  ( initEDB
  , finalEDB
  , ancAncKnowledge
  , parKnowledge
  , parAncKnowledge
  , finalLinearProvEDB
  , finalNonLinearProvEDB
  ) where

import Protolude hiding (Set, head)

import           Data.Maybe (fromJust)
import qualified Data.Vector.Sized as V
import qualified Data.List.NonEmpty as NE
import           Data.List

import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Class as KB
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.Provenance
import           Language.Exalog.SrcLoc

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
parentKB = KB.fromList $ Knowledge KnowABase parPred . fmap symbol <$> parentTuples

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
ancestorKB = KB.fromList $ Knowledge KnowABase ancPred . fmap symbol <$> ancestorTuples

initEDB :: Set 'ABase
initEDB = parentKB

finalEDB :: Set 'ABase
finalEDB = parentKB <> ancestorKB

-- EDB construction with Provenance

-- anc(X,Z) :- anc(X,Y), anc(Y,Z).
ancAncClause :: Term -> Term -> Term -> Term -> Term -> Term -> Clause ('AProvenance 'ABase)
ancAncClause h1 h2 b1 b2 b3 b4 = Clause
  (ClAProvenance (ClABase NoSpan))
  (ancProv h1 h2)
  (NE.fromList [ ancProv b1 b2, ancProv b3 b4 ])

ancAncKnowledge :: [ Text ] -> Knowledge ('AProvenance 'ABase)
ancAncKnowledge t =
  mkKnowledge
    (ancAncClause
      (TSym (SymText $ head t))
      (TSym (SymText $ t !! 1))
      (TSym (SymText $ t !! 2))
      (TSym (SymText $ t !! 3))
      (TSym (SymText $ t !! 4))
      (TSym (SymText $ t !! 5)))
    ancPredProv
    (V.fromTuple (SymText $ head t, SymText $ t !! 1))

ancAncTuples :: [ [ Text ] ]
ancAncTuples =
  [ [ "Jean-Pierre" , "Mistral" , "Jean-Pierre", "Laurent", "Laurent", "Mistral"]
  , [ "Omer" , "Emir" , "Omer", "Orhan", "Orhan", "Emir"]
  , [ "Omer" , "Hulusi" , "Omer", "Orhan", "Orhan", "Hulusi"]
  , [ "Omer" , "Mistral" , "Omer", "Orhan", "Orhan", "Mistral"]
  , [ "Omer" , "Nilufer" , "Omer", "Orhan", "Orhan", "Nilufer"]
  , [ "Orhan" , "Emir" , "Orhan", "Hulusi", "Hulusi", "Emir"]
  , [ "Orhan" , "Mistral" , "Orhan", "Nilufer", "Nilufer", "Mistral"]
  , [ "Simone" , "Mistral" , "Simone", "Laurent", "Laurent", "Mistral"]
  -- The diff from linear
  , [ "Omer" , "Emir" , "Omer", "Hulusi", "Hulusi", "Emir"]
  , [ "Omer" , "Mistral" , "Omer", "Nilufer", "Nilufer", "Mistral"]
  ]

ancAncEDB :: Set ('AProvenance 'ABase)
ancAncEDB = KB.fromList $ Data.List.map ancAncKnowledge ancAncTuples

-- anc(X,Y) :- par(X,Y).
parClause :: Term -> Term -> Term -> Term -> Clause ('AProvenance 'ABase)
parClause h1 h2 b1 b2 = Clause
  (ClAProvenance (ClABase NoSpan))
  (ancProv h1 h2)
  (NE.fromList [ parProv b1 b2 ])

parKnowledge :: [ Text ] -> Knowledge ('AProvenance 'ABase)
parKnowledge t =
  mkKnowledge
    (parClause
      (TSym (SymText (head t)))
      (TSym (SymText (t !! 1)))
      (TSym (SymText (t !! 2)))
      (TSym (SymText (t !! 3))))
    ancPredProv
    (V.fromTuple (SymText (head t), SymText (t!!1)))

parTuples :: [ [ Text ] ]
parTuples =
  [ [ "Hulusi"      , "Emir"    , "Hulusi"      , "Emir"    ]
  , [ "Jean-Pierre" , "Laurent" , "Jean-Pierre" , "Laurent" ]
  , [ "Laurent"     , "Mistral" , "Laurent"     , "Mistral" ]
  , [ "Nazli"       , "Emir"    , "Nazli"       , "Emir"    ]
  , [ "Nilufer"     , "Mistral" , "Nilufer"     , "Mistral" ]
  , [ "Omer"        , "Orhan"   , "Omer"        , "Orhan"   ]
  , [ "Orhan"       , "Hulusi"  , "Orhan"       , "Hulusi"  ]
  , [ "Orhan"       , "Nilufer" , "Orhan"       , "Nilufer" ]
  , [ "Simone"      , "Laurent" , "Simone"      , "Laurent" ]
  ]

parEDB :: Set ('AProvenance 'ABase)
parEDB = KB.fromList $ Data.List.map parKnowledge parTuples

-- anc(X,Z) :- par(X,Y), anc(Y,Z).
parAncClause :: Term -> Term -> Term -> Term -> Term -> Term -> Clause ('AProvenance 'ABase)
parAncClause h1 h2 b1 b2 b3 b4 = Clause
  (ClAProvenance (ClABase NoSpan))
  (ancProv h1 h2)
  (NE.fromList [ parProv b1 b2, ancProv b3 b4 ])

parAncKnowledge :: [ Text ]-> Knowledge ('AProvenance 'ABase)
parAncKnowledge t =
  mkKnowledge
    (parAncClause
      (TSym (SymText $ head t))
      (TSym (SymText $ t !! 1))
      (TSym (SymText $ t !! 2))
      (TSym (SymText $ t !! 3))
      (TSym (SymText $ t !! 4))
      (TSym (SymText $ t !! 5)))
    ancPredProv
    (V.fromTuple (SymText $ head t, SymText $ t !! 1))

parAncTuples :: [ [ Text ] ]
parAncTuples =
  [ [ "Jean-Pierre" , "Mistral" , "Jean-Pierre", "Laurent", "Laurent", "Mistral"]
  , [ "Omer" , "Emir" , "Omer", "Orhan", "Orhan", "Emir"]
  , [ "Omer" , "Hulusi" , "Omer", "Orhan", "Orhan", "Hulusi"]
  , [ "Omer" , "Mistral" , "Omer", "Orhan", "Orhan", "Mistral"]
  , [ "Omer" , "Nilufer" , "Omer", "Orhan", "Orhan", "Nilufer"]
  , [ "Orhan" , "Emir" , "Orhan", "Hulusi", "Hulusi", "Emir"]
  , [ "Orhan" , "Mistral" , "Orhan", "Nilufer", "Nilufer", "Mistral"]
  , [ "Simone" , "Mistral" , "Simone", "Laurent", "Laurent", "Mistral"]
  ]

parAncEDB :: Set ('AProvenance 'ABase)
parAncEDB = KB.fromList $ parAncKnowledge <$> parAncTuples

initProvEDB :: Set ('AProvenance 'ABase)
initProvEDB = KB.atEach decorate initEDB

finalLinearProvEDB :: Set ('AProvenance 'ABase)
finalLinearProvEDB = initProvEDB <> parAncEDB <> parEDB

finalNonLinearProvEDB :: Set ('AProvenance 'ABase)
finalNonLinearProvEDB = initProvEDB <> ancAncEDB <> parEDB
