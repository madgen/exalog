{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Exalog.Dataflow
  ( PositiveFlowGr
  , FlowSource(..)
  , FlowSink(..)
  , analysePositiveFlow
  , nearestCoveringPositives
  ) where

import Protolude hiding (head, sym)

import           Data.List (nub)
import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Sized as V

import Language.Exalog.Core
import Language.Exalog.Renamer

--------------------------------------------------------------------------------
-- Exported data types
--------------------------------------------------------------------------------

data PositiveFlowGr = PositiveFlowGr (P.Gr Node ()) (M.Map Node Int)

data FlowSink   = FSinkLiteral   LiteralID Int | FSinkPredicate PredicateID Int
data FlowSource = FSourceLiteral LiteralID Int | FSourceConstant Constant

--------------------------------------------------------------------------------
-- Main operations
--------------------------------------------------------------------------------

analysePositiveFlow :: Program ('ARename ann) -> PositiveFlowGr
analysePositiveFlow pr = PositiveFlowGr (Gr.mkGraph lnodes ledges) nodeDict
  where
  edges = nub $ programEdges pr
  lnodes = zip [0..] $ nub (map fst edges ++ map snd edges)
  nodeDict = M.fromList $ map swap lnodes

  ledges = (\(a,b) -> (a,b,()))
         . bimap (nodeDict M.!) (nodeDict M.!)
       <$> edges

-- |Finds the nearest positive parameters of predicates or constants that
-- flow into a given literal argument. The results together cover the
-- domain of values the target can take.
nearestCoveringPositives :: PositiveFlowGr -> FlowSink -> Maybe [ FlowSource ]
nearestCoveringPositives (PositiveFlowGr gr dict) fSink =
  concatMap (go []) . Gr.pre' <$> mContext
  where
  mContext = Gr.context gr <$> toNode fSink `M.lookup` dict

  go :: [ Gr.Node ] -> Gr.Node -> [ FlowSource ]
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

data Constant = CSym Sym | CWild deriving (Eq, Ord)

data Node =
    NPredicate { _predicateID :: PredicateID, _paramIndex :: Int }
  | NLiteral   { _literalID   :: LiteralID  , _paramIndex :: Int }
  | NConstant  { _constant    :: Constant }
  deriving (Eq, Ord)

type Edge = (Node, Node)

toNode :: FlowSink -> Node
toNode (FSinkLiteral   litID  ix) = NLiteral   litID  ix
toNode (FSinkPredicate predID ix) = NPredicate predID ix

--------------------------------------------------------------------------------
-- Feature extraction
--------------------------------------------------------------------------------

programEdges :: Program ('ARename ann) -> [ Edge ]
programEdges pr@Program{..} = concatMap (clauseEdges intentionals) clauses
  where
  intentionals = S.fromList . map predicateID . findIntentionals $ pr

clauseEdges :: S.Set PredicateID -> Clause ('ARename ann) -> [ Edge ]
clauseEdges intentionals Clause{..} = join . evalSideways intentionals $ do
  handleHeadLiteral head

  traverse handleBodyLiteral (NE.toList body)

handleHeadLiteral :: Literal ('ARename ann) -> Sideways ()
handleHeadLiteral Literal{..} =
  forM_ (zip [0..] $ V.toList terms) $ \case
    (ix, TVar var) -> updateBinder var (NPredicate (predicateID predicate) ix)
    _              -> pure ()

handleBodyLiteral :: Literal ('ARename ann) -> Sideways [ Edge ]
handleBodyLiteral Literal{..} = do
  edgess <- forM (zip [0..] $ V.toList terms) $ \(ix, term) -> do
    -- Bother with predicate node as a destination only if it is
    -- intentional.
    dsts <- getPredNode (predicateID predicate) ix

    case term of
      TVar var -> do
        mSrc <- getBinder var

        let litNode = NLiteral (literalID annotation) ix
        when (polarity == Positive) $ updateBinder var litNode

        pure [ (src, dst) | src <- toList mSrc, dst <- litNode : dsts ]
      TSym sym -> pure [ (NConstant (CSym sym), dst) | dst <- dsts ]
      TWild    -> pure [ (NConstant CWild     , dst) | dst <- dsts ]

  pure $ mconcat edgess

--------------------------------------------------------------------------------
-- Monadic actions
--------------------------------------------------------------------------------

newtype SidewaysSt = SidewaysSt { _binderMap :: M.Map Var Node }

type Sideways = ReaderT (S.Set PredicateID) (State SidewaysSt)

initSidewaysSt :: SidewaysSt
initSidewaysSt = SidewaysSt M.empty

evalSideways :: S.Set PredicateID -> Sideways a -> a
evalSideways intentionalIDs = (`evalState` initSidewaysSt)
                            . (`runReaderT` intentionalIDs)

getPredNode :: PredicateID -> Int -> Sideways [ Node ]
getPredNode id ix = do
  intentionalIDs <- ask
  pure [ NPredicate id ix | id `S.member` intentionalIDs ]

getBinder :: Var -> Sideways (Maybe Node)
getBinder var = lift $ M.lookup var . _binderMap <$> get

updateBinder :: Var -> Node -> Sideways ()
updateBinder var binder = lift $
  modify (\st -> st {_binderMap = M.insert var binder $ _binderMap st})
