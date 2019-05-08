{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Exalog.Dataflow
  ( PositiveFlowGr
  , FlowSource(..)
  , FlowSink(..)
  , Constant(..)
  , analysePositiveFlow
  , nearestCoveringPositives
  , HasEdge(..)
  ) where

import Protolude hiding (head, sym, pred)

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.PatriciaTree as P
import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Set as S
import qualified Data.Vector.Sized as V

import Language.Exalog.Core
import Language.Exalog.Renamer ()

--------------------------------------------------------------------------------
-- Exported data types
--------------------------------------------------------------------------------

data PositiveFlowGr ann =
  PositiveFlowGr (P.Gr (Node ann) ()) (BM.Bimap (Node ann) Gr.Node)

data FlowSink ann =
    FSinkLiteral   (Literal ('ARename ann)) Int
  | FSinkPredicate (PredicateBox ('ARename ann)) Int

data FlowSource ann =
    FSourceLiteral (Literal ('ARename ann)) Int
  | FSourceConstant Constant

data Constant = CSym Sym | CWild deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Main operations
--------------------------------------------------------------------------------

analysePositiveFlow :: IdentifiableAnn (PredicateAnn ann) a => Ord a
                    => IdentifiableAnn (LiteralAnn   ann) b => Ord b
                    => Program ('ARename ann) -> PositiveFlowGr ann
analysePositiveFlow pr = PositiveFlowGr (Gr.mkGraph lnodes ledges) nodeDict
  where
  edges = nub $ programEdges pr
  lnodes = zip [0..] $ nub (map fst edges ++ map snd edges)
  nodeDict = BM.fromList $ map swap lnodes

  ledges = (\(a,b) -> (a,b,()))
         . bimap (nodeDict BM.!) (nodeDict BM.!)
       <$> edges

-- |Finds the nearest positive parameters of predicates or constants that
-- flow into a given literal argument. The results together cover the
-- domain of values the target can take.
nearestCoveringPositives :: forall ann a b
                          . IdentifiableAnn (PredicateAnn ann) a => Ord a
                         => IdentifiableAnn (LiteralAnn   ann) b => Ord b
                         => PositiveFlowGr ann
                         -> FlowSink ann
                         -> Maybe (NE.NonEmpty (FlowSource ann))
nearestCoveringPositives (PositiveFlowGr gr dict) fSink = do
  context <- mContext
  nonEmpty . concatMap (go []) . Gr.pre' $ context
  where
  mContext = Gr.context gr <$> toNode fSink `BM.lookup` dict

  go :: [ Gr.Node ] -> Gr.Node -> [ FlowSource ann ]
  go visitedNodes node
    | node `elem` visitedNodes = mempty
    | context <- Gr.context gr node =
      case Gr.lab' context of
        NConstant constant -> pure $ FSourceConstant constant
        NLiteral litID ix  -> pure $ FSourceLiteral litID ix
        NPredicate _ _     ->
          concatMap (go (node : visitedNodes)) . Gr.pre' $ context

--------------------------------------------------------------------------------
-- Internal data types
--------------------------------------------------------------------------------

data Node ann =
    NPredicate { _predicate :: PredicateBox ('ARename ann), _paramIndex :: Int }
  | NLiteral   { _literal   :: Literal      ('ARename ann), _paramIndex :: Int }
  | NConstant  { _constant  :: Constant }

deriving instance
  ( Show (PredicateAnn ann)
  , Show (LiteralAnn   ann)
  ) => Show (Node ann)
deriving instance
  ( IdentifiableAnn (PredicateAnn ann) a, Eq a
  , IdentifiableAnn (LiteralAnn   ann) b, Eq b
  ) => Eq (Node ann)
deriving instance
  ( IdentifiableAnn (PredicateAnn ann) a, Ord a
  , IdentifiableAnn (LiteralAnn   ann) b, Ord b
  ) => Ord (Node ann)

type Edge ann = (Node ann, Node ann)

toNode :: FlowSink ann -> Node ann
toNode (FSinkLiteral   lit  ix) = NLiteral   lit  ix
toNode (FSinkPredicate pBox ix) = NPredicate pBox ix

--------------------------------------------------------------------------------
-- Feature extraction
--------------------------------------------------------------------------------

programEdges :: IdentifiableAnn (PredicateAnn ann) a => Ord a
             => Program ('ARename ann) -> [ Edge ann ]
programEdges pr@Program{..} = concatMap (clauseEdges intentionals) clauses
  where
  intentionals = S.fromList . findIntentionals $ pr

clauseEdges :: IdentifiableAnn (PredicateAnn ann) a => Ord a
            => S.Set (PredicateBox ('ARename ann))
            -> Clause ('ARename ann)
            -> [ Edge ann ]
clauseEdges intentionals Clause{..} = join . evalSideways intentionals $ do
  handleHeadLiteral head

  traverse handleBodyLiteral (NE.toList body)

handleHeadLiteral :: IdentifiableAnn (PredicateAnn ann) a => Ord a
                  => Literal ('ARename ann) -> Sideways ann ()
handleHeadLiteral Literal{..} =
  forM_ (zip [0..] $ V.toList terms) $ \case
    (ix, TVar var) -> updateBinder var (NPredicate (PredicateBox predicate) ix)
    _              -> pure ()

handleBodyLiteral :: IdentifiableAnn (PredicateAnn ann) a => Ord a
                  => Literal ('ARename ann) -> Sideways ann [ Edge ann ]
handleBodyLiteral lit@Literal{..} = do
  edgess <- forM (zip [0..] $ V.toList terms) $ \(ix, term) -> do
    -- Bother with predicate node as a destination only if it is
    -- intentional.
    dsts <- getPredNode (PredicateBox predicate) ix

    case term of
      TVar var -> do
        mSrc <- getBinder var

        let litNode = NLiteral lit ix
        when (polarity == Positive) $ updateBinder var litNode

        pure [ (src, dst) | src <- toList mSrc, dst <- litNode : dsts ]
      TSym sym -> pure [ (NConstant (CSym sym), dst) | dst <- dsts ]
      TWild    -> pure [ (NConstant CWild     , dst) | dst <- dsts ]

  pure $ mconcat edgess

--------------------------------------------------------------------------------
-- Monadic actions
--------------------------------------------------------------------------------

newtype SidewaysSt ann = SidewaysSt { _binderMap :: M.Map Var (Node ann) }

type Sideways ann =
  ReaderT (S.Set (PredicateBox ('ARename ann))) (State (SidewaysSt ann))

initSidewaysSt :: SidewaysSt ann
initSidewaysSt = SidewaysSt M.empty

evalSideways :: S.Set (PredicateBox ('ARename ann)) -> Sideways ann a -> a
evalSideways intentionals = (`evalState` initSidewaysSt)
                          . (`runReaderT` intentionals)

getPredNode :: IdentifiableAnn (PredicateAnn ann) a => Ord a
            => PredicateBox ('ARename ann) -> Int -> Sideways ann [ Node ann ]
getPredNode pBox ix = do
  intentionals <- ask
  pure [ NPredicate pBox ix | pBox `S.member` intentionals ]

getBinder :: Var -> Sideways ann (Maybe (Node ann))
getBinder var = lift $ M.lookup var . _binderMap <$> get

updateBinder :: Var -> Node ann -> Sideways ann ()
updateBinder var binder = lift $
  modify (\st -> st {_binderMap = M.insert var binder $ _binderMap st})

--------------------------------------------------------------------------------
-- Useful for testing
--------------------------------------------------------------------------------

class HasEdge f g ann where
  isAnEdge :: PositiveFlowGr ann -> (f ann, Int) -> (g ann, Int) -> Bool

instance ( IdentifiableAnn (PredicateAnn  ann) a, Ord a
         , IdentifiableAnn (LiteralAnn    ann) b, Ord b
         ) => HasEdge Literal Literal ann where
  isAnEdge (PositiveFlowGr flowGr nodeDict) (lit1, ix1) (lit2, ix2) =
    any match (Gr.edges flowGr)
    where
    match :: (Gr.Node, Gr.Node) -> Bool
    match (node1, node2) = matchNode nodeDict (matchLitNode lit1 ix1) node1
                        && matchNode nodeDict (matchLitNode lit2 ix2) node2

instance ( IdentifiableAnn (PredicateAnn  ann) a, Ord a
         , IdentifiableAnn (LiteralAnn    ann) b, Ord b
         ) => HasEdge PredicateBox Literal ann where
  isAnEdge (PositiveFlowGr flowGr nodeDict) (pBox, ix1) (lit, ix2) =
    any match (Gr.edges flowGr)
    where
    match :: (Gr.Node, Gr.Node) -> Bool
    match (node1, node2) = matchNode nodeDict (matchPredNode pBox ix1) node1
                        && matchNode nodeDict (matchLitNode  lit  ix2) node2

instance ( IdentifiableAnn (PredicateAnn  ann) a, Ord a
         , IdentifiableAnn (LiteralAnn    ann) b, Ord b
         ) => HasEdge Literal PredicateBox ann where
  isAnEdge (PositiveFlowGr flowGr nodeDict) (lit, ix1) (pBox, ix2) =
    any match (Gr.edges flowGr)
    where
    match :: (Gr.Node, Gr.Node) -> Bool
    match (node1, node2) = matchNode nodeDict (matchLitNode lit ix1) node1
                        && matchNode nodeDict (matchPredNode  pBox  ix2) node2


instance ( IdentifiableAnn (PredicateAnn ann) a, Ord a
         , IdentifiableAnn (LiteralAnn   ann) b, Ord b
         ) => HasEdge PredicateBox PredicateBox ann where
  isAnEdge (PositiveFlowGr flowGr nodeDict) (pBox1, ix1) (pBox2, ix2) =
    any match (Gr.edges flowGr)
    where
    match :: (Gr.Node, Gr.Node) -> Bool
    match (node1, node2) = matchNode nodeDict (matchPredNode pBox1 ix1) node1
                        && matchNode nodeDict (matchPredNode pBox2 ix2) node2

instance ( IdentifiableAnn (PredicateAnn ann) a, Ord a
         , IdentifiableAnn (LiteralAnn   ann) b, Ord b
         ) => HasEdge (Const Constant) PredicateBox ann where
  isAnEdge (PositiveFlowGr flowGr nodeDict) (Const constant, _) (pBox2, ix2) =
    any match (Gr.edges flowGr)
    where
    match :: (Gr.Node, Gr.Node) -> Bool
    match (node1, node2) = matchNode nodeDict (matchConstNode constant) node1
                        && matchNode nodeDict (matchPredNode pBox2 ix2) node2

matchConstNode :: Constant -> Node ann -> Bool
matchConstNode constant NConstant{..} = _constant == constant
matchConstNode _        _             = False

matchPredNode :: IdentifiableAnn (PredicateAnn ann) a => Eq a
              => PredicateBox ann -> Int -> Node ann -> Bool
matchPredNode pred ix NPredicate{..} = ix == _paramIndex && peel _predicate == pred
matchPredNode _    _  _              = False

matchLitNode :: IdentifiableAnn (PredicateAnn ann) a => Eq a
             => IdentifiableAnn (LiteralAnn   ann) b => Eq b
             => Literal ann -> Int -> Node ann -> Bool
matchLitNode lit ix NLiteral{..} = ix == _paramIndex && peel _literal == lit
matchLitNode _   _  _            = False

matchNode :: IdentifiableAnn (PredicateAnn ann) a => Ord a
          => IdentifiableAnn (LiteralAnn   ann) b => Ord b
          => BM.Bimap (Node ann) Gr.Node
          -> (Node ann -> Bool)
          -> Gr.Node
          -> Bool
matchNode nodeDict f node = maybe False f (node `BM.lookupR` nodeDict)

--------------------------------------------------------------------------------
-- Useful instances
--------------------------------------------------------------------------------

deriving instance ( Show (PredicateAnn ann)
                  , Show (LiteralAnn   ann)
                  ) => Show (FlowSink   ann)
deriving instance ( Show (PredicateAnn ann)
                  , Show (LiteralAnn   ann)
                  ) => Show (FlowSource ann)

deriving instance ( IdentifiableAnn (PredicateAnn ann) a, Eq a
                  , IdentifiableAnn (LiteralAnn   ann) a, Eq a
                  ) => Eq (FlowSink   ann)
deriving instance ( IdentifiableAnn (PredicateAnn ann) a, Eq a
                  , IdentifiableAnn (LiteralAnn   ann) a, Eq a
                  ) => Eq (FlowSource ann)

