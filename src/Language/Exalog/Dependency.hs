{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.Dependency
  ( DependencyGr
  , dependencyGr
  ) where

import Protolude hiding (pred)

import           Data.Bifunctor (bimap)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as P
import           Data.List (lookup, nub)
import qualified Data.List.NonEmpty as NE

import           Language.Exalog.Core
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB

type DependencyGr (a :: AnnType) = P.Gr (PredicateBox a) Polarity

newtype instance PredicateAnn ('ADependency a) =
  PredADependency (PredicateAnn a)
newtype instance LiteralAnn ('ADependency a)   = LitADependency (LiteralAnn a)
newtype instance ClauseAnn  ('ADependency a)   = ClADependency (ClauseAnn a)
data    instance ProgramAnn ('ADependency a)   =
  ProgADependency (DependencyGr a) (ProgramAnn a)
newtype instance KnowledgeAnn ('ADependency a) =
    KnowADependency (KnowledgeAnn a)

instance KB.KnowledgeMaker ann => KB.KnowledgeMaker ('ADependency ann) where
  mkKnowledge predicate syms = KB.Knowledge (KnowADependency (KB._annotation (KB.mkKnowledge (peel predicate) syms))) predicate syms

instance SpannableAnn (PredicateAnn a) => SpannableAnn (PredicateAnn ('ADependency a)) where
  annSpan (PredADependency ann) = annSpan ann
instance SpannableAnn (LiteralAnn a) => SpannableAnn (LiteralAnn ('ADependency a)) where
  annSpan (LitADependency ann) = annSpan ann
instance SpannableAnn (ClauseAnn a) => SpannableAnn (ClauseAnn ('ADependency a)) where
  annSpan (ClADependency ann) = annSpan ann
instance SpannableAnn (ProgramAnn a) => SpannableAnn (ProgramAnn ('ADependency a)) where
  annSpan (ProgADependency _ ann) = annSpan ann

instance IdentifiableAnn (PredicateAnn ann) b
    => IdentifiableAnn (PredicateAnn ('ADependency ann)) b where
  idFragment (PredADependency rest) = idFragment rest
instance IdentifiableAnn (LiteralAnn ann) b
    => IdentifiableAnn (LiteralAnn ('ADependency ann)) b where
  idFragment (LitADependency rest) = idFragment rest
instance IdentifiableAnn (ClauseAnn ann) b
    => IdentifiableAnn (ClauseAnn ('ADependency ann)) b where
  idFragment (ClADependency rest) = idFragment rest
instance IdentifiableAnn (ProgramAnn ann) b
    => IdentifiableAnn (ProgramAnn ('ADependency ann)) b where
  idFragment (ProgADependency _ rest) = idFragment rest

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
    Literal { _annotation = peelA _annotation
            , _predicate  = peel  _predicate
            , ..}
instance PeelableAST (PredicateBox ('ADependency a)) where
  peel (PredicateBox pred) = PredicateBox $ peel pred

instance DecorableAnn PredicateAnn 'ADependency where
  decorA = PredADependency
instance DecorableAnn LiteralAnn 'ADependency where
  decorA = LitADependency
instance DecorableAnn ClauseAnn 'ADependency where
  decorA = ClADependency

instance DecorableAST (Literal a) 'ADependency where
  decorate Literal{..} =
    Literal { _annotation = decorA   _annotation
            , _predicate  = decorate _predicate
            , ..}

instance {-# OVERLAPPING #-}
         Identifiable (PredicateAnn a) b
      => DecorableAST (Program a) 'ADependency where
  decorate pr@Program{..} =
    Program { _annotation = ProgADependency (mkDependencyGr pr) _annotation
            , _strata     = stratumOver (map decorate) <$> _strata
            , _queries    = map decorate _queries
            }

dependencyGr :: Program ('ADependency a) -> DependencyGr a
dependencyGr Program{_annotation = ProgADependency gr _} = gr

mkDependencyGr :: forall a b. Identifiable (PredicateAnn a) b
               => Program a -> DependencyGr a
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
    Clause{_head = Literal{_predicate = headPred}, _body = body}
      <- concatMap _unStratum _strata
    Literal{_polarity = pol, _predicate = bodyPred} <- NE.toList body
    case bimap findID findID (bodyPred, headPred) of
      (Just src, Just dst) -> return (src, dst, pol)
      _ -> panic "Impossible: predicate is not in the program."
