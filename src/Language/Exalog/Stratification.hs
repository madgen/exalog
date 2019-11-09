{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           Language.Exalog.SrcLoc (SrcSpan(NoSpan))
import           Language.Exalog.Dependency
import           Language.Exalog.Logger

-- |Returns a stratified program in the form of a list to be executed in
-- order.
stratify :: forall a b. Identifiable (PredicateAnn a) b
         => Program ('ADependency a) -> Logger (Program a)
stratify pr@Program{} = do
  strata <- sequence $ do
      comp <- sccs
      let polarities = sccPolarities comp
      if Negative `elem` polarities
        then pure $
          scold NoSpan "Stratification failed due to cyclic use of negation."
        else do
          let cls = concatMap (search peeledPr) . findPreds depGrDict $ comp
          guard (not . null $ cls)
          pure $ pure cls
  pure $ peeledPr {_strata = Stratum <$> strata}
  where
  depGr = dependencyGr pr
  depGrDict = G.labNodes depGr

  peeledPr = peel pr

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
