{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Exalog.Stratification
  ( stratify
  ) where

import Protolude hiding (head)

import qualified Data.Graph.Inductive.Graph as G
import           Data.Graph.Inductive.Query.DFS (condensation, topsort)
import           Data.List (lookup)
import           Data.Maybe (fromJust)

import           Language.Exalog.Core
import           Language.Exalog.Dependency

-- |Returns a stratified program in the form of a list to be executed in
-- order.
stratify :: Eq (PredicateAnn a) =>
         Program ('ADependency a) -> Either Text [ Program a ]
stratify p@Program{annotation = ann} = sequence $ do
  comp <- sccs
  let polarities = sccPolarities comp
  return $ if Negative `elem` polarities
    then Left "There is cyclic negation."
    else
      let clauses = concatMap (search . peel $ p) . findPreds depGrDict $ comp
      in Right $ Program (peelA ann) clauses
  where
  depGr = dependencyGr p
  depGrDict = G.labNodes depGr

  -- Find SCCs like `Data.Graph.Inductive.Query.DFS.scc` but in topological
  -- order.
  sccs :: [ [ G.Node ] ]
  sccs =
    let gr = condensation depGr
    in findPreds (G.labNodes gr) (topsort gr)

  findPreds :: [ G.LNode a ] -> [ G.Node ] -> [ a ]
  findPreds nodeDict = map (fromJust . flip lookup nodeDict)

  sccPolarities :: [ G.Node ] -> [ Polarity ]
  sccPolarities nodes = map (\(_,_,lab) -> lab)
                      . filter (\(_,dst,_) -> dst `elem` nodes)
                      . concatMap (G.out' . fromJust . fst . flip G.match depGr)
                      $ nodes
