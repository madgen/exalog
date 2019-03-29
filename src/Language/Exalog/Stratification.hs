{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Language.Exalog.Logger

-- |Returns a stratified program in the form of a list to be executed in
-- order.
stratify :: forall a b. Identifiable (PredicateAnn a) b
         => Program ('ADependency a) -> Logger [ Program a ]
stratify pr@Program{annotation = ann} = sequence $ do
  comp <- sccs
  let polarities = sccPolarities comp
  if Negative `elem` polarities
    then pure $
      scold Nothing "Stratification failed due to cyclic use of negation."
    else do
      let peeledPr = peel pr
      let cls = concatMap (search $ peeledPr) . findPreds depGrDict $ comp
      guard (not . null $ cls)
      pure $ pure $
        Program (peelA ann) cls (queries cls (queryPreds peeledPr))
  where
  depGr = dependencyGr pr
  depGrDict = G.labNodes depGr

  queries :: [ Clause a ] -> [ PredicateBox a ] -> [ PredicateBox a ]
  queries cls qps
    -- A query must be alone in its SCC
    | [ Clause{head = lit} ] <- cls
    , pbox <- predicateBox lit
    , pbox `elem` qps = [ pbox ]
    | otherwise = []

  -- Find SCCs like `Data.Graph.Inductive.Query.DFS.scc` but in topological
  -- order.
  sccs :: [ [ G.Node ] ]
  sccs =
    let gr = condensation depGr
    in findPreds (G.labNodes gr) (topsort gr)

  findPreds :: forall c. [ G.LNode c ] -> [ G.Node ] -> [ c ]
  findPreds nodeDict = map (fromJust . flip lookup nodeDict)

  sccPolarities :: [ G.Node ] -> [ Polarity ]
  sccPolarities nodes = map (\(_,_,lab) -> lab)
                      . filter (\(_,dst,_) -> dst `elem` nodes)
                      . concatMap (G.out' . fromJust . fst . flip G.match depGr)
                      $ nodes
