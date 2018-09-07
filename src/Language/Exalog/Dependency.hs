{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Exalog.Dependency
  ( DependencyGr
  , dependencyGr
  ) where

import Protolude

import           Data.Bifunctor (bimap)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as P
import           Data.List (lookup, nub)
import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core

type DependencyGr (a :: AnnType) = P.Gr (PredicateBox a) Polarity

newtype instance PredicateAnn ('ADependency a) =
  PredADependency (PredicateAnn a)
newtype instance LiteralAnn ('ADependency a)   = LitADependency (LiteralAnn a)
newtype instance ClauseAnn  ('ADependency a)   = ClADependency (ClauseAnn a)
data    instance ProgramAnn ('ADependency a)   =
  ProgADependency (DependencyGr a) (ProgramAnn a)

instance PeelableAnn PredicateAnn 'ADependency where
  peelA (PredADependency a) = a
instance PeelableAnn LiteralAnn 'ADependency where
  peelA (LitADependency a) = a
instance PeelableAnn ClauseAnn 'ADependency where
  peelA (ClADependency a) = a
instance PeelableAnn ProgramAnn 'ADependency where
  peelA (ProgADependency _ a) = a

instance PeelableAST (Literal ('ADependency a)) where
  peel Literal{..} =
    Literal { annotation = peelA annotation
            , predicate = peel predicate
            , ..}

instance DecorableAnn PredicateAnn 'ADependency where
  decorA = PredADependency
instance DecorableAnn LiteralAnn 'ADependency where
  decorA = LitADependency
instance DecorableAnn ClauseAnn 'ADependency where
  decorA = ClADependency

instance DecorableAST (Literal a) 'ADependency where
  decorate Literal{..} =
    Literal { annotation = decorA annotation
            , predicate = decorate predicate
            , ..}

instance {-# OVERLAPPING #-}
         Eq (PredicateAnn a)
      => DecorableAST (Program a) 'ADependency where
  decorate pr@Program{..} =
    Program { annotation = ProgADependency (mkDependencyGr pr) annotation
            , clauses = map decorate clauses
            }

dependencyGr :: Program ('ADependency a) -> DependencyGr a
dependencyGr Program{annotation = ProgADependency gr _} = gr

mkDependencyGr :: forall a. Eq (PredicateAnn a) => Program a -> DependencyGr a
mkDependencyGr pr@Program{..} = G.mkGraph nodes (nub edges)
  where
  nodeDict :: [ (PredicateBox a, G.Node) ]
  nodeDict = zip (predicates pr) [1..]

  findID :: Predicate n a -> Maybe G.Node
  findID p = lookup (PredicateBox p) nodeDict

  nodes :: [ G.LNode (PredicateBox a) ]
  nodes = map (\(a,b) -> (b,a)) nodeDict

  edges :: [ G.LEdge Polarity ]
  edges = do
    Clause{head = Literal{predicate = headPred}, body = body} <- clauses
    Literal{polarity = pol, predicate = bodyPred} <- NE.toList body
    case bimap findID findID (bodyPred, headPred) of
      (Just src, Just dst) -> return (src, dst, pol)
      _ -> panic "Impossible: predicate is not in the program."
