{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Exalog.Dataflow
  ( PositiveFlowGr
  , Direction(..)
  , type Reverse
  , analysePositiveFlow
  , reversePositiveFlow
  ) where

import Protolude hiding (head, sym)

import           Data.List (nub)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.IntSet as IS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Vector.Sized as V

import Language.Exalog.Core
import Language.Exalog.Renamer

--------------------------------------------------------------------------------
-- Exported data types
--------------------------------------------------------------------------------

newtype PositiveFlowGr (dir :: Direction) = PositiveFlowGr (P.Gr Node ())

data Direction = Forward | Backward

type family Reverse (dir :: Direction) where
  Reverse 'Forward  = 'Backward
  Reverse 'Backward = 'Forward

--------------------------------------------------------------------------------
-- Main operations
--------------------------------------------------------------------------------

analysePositiveFlow :: Program ('ARename ann) -> PositiveFlowGr 'Forward
analysePositiveFlow pr = PositiveFlowGr $ G.mkGraph lnodes ledges
  where
  edges = nub $ programEdges pr
  lnodes = zip [0..] $ nub (map fst edges ++ map snd edges)
  nodeDict = M.fromList $ map swap lnodes

  ledges = (\(a,b) -> (a,b,()))
         . bimap (nodeDict M.!) (nodeDict M.!)
       <$> edges

reversePositiveFlow :: PositiveFlowGr dir -> PositiveFlowGr (Reverse dir)
reversePositiveFlow (PositiveFlowGr gr) = PositiveFlowGr
                                        . G.mkGraph (G.labNodes gr)
                                        $ (\(src,dst,lab) -> (dst,src,lab))
                                      <$> G.labEdges gr

--------------------------------------------------------------------------------
-- Internal data types
--------------------------------------------------------------------------------

data Constant = CSym Sym | CWild deriving (Eq, Ord)

data Node =
    NPredicate { _predicateID :: Int, _paramIndex :: Int }
  | NLiteral   { _literalID   :: Int, _paramIndex :: Int }
  | NConstant  { _constant    :: Constant }
  deriving (Eq, Ord)

type Edge = (Node, Node)

--------------------------------------------------------------------------------
-- Feature extraction
--------------------------------------------------------------------------------

programEdges :: Program ('ARename ann) -> [ Edge ]
programEdges pr@Program{..} = concatMap (clauseEdges intentionals) clauses
  where
  intentionals = IS.fromList . map uniqID . findIntentionals $ pr

clauseEdges :: IS.IntSet -> Clause ('ARename ann) -> [ Edge ]
clauseEdges intentionals Clause{..} = join . evalSideways intentionals $ do
  handleHeadLiteral head

  traverse handleBodyLiteral (NE.toList body)

handleHeadLiteral :: Literal ('ARename ann) -> Sideways ()
handleHeadLiteral Literal{..} =
  forM_ (zip [0..] $ V.toList terms) $ \case
    (ix, TVar var) -> updateBinder var (NPredicate (uniqID predicate) ix)
    _              -> pure ()

handleBodyLiteral :: Literal ('ARename ann) -> Sideways [ Edge ]
handleBodyLiteral Literal{..} = do
  edgess <- forM (zip [0..] $ V.toList terms) $ \(ix, term) -> do
    -- Bother with predicate node as a destination only if it is
    -- intentional.
    dsts <- getPredNode (uniqID predicate) ix

    case term of
      TVar var -> do
        mSrc <- getBinder var

        let litNode = NLiteral (uniqID annotation) ix
        when (polarity == Positive) $ updateBinder var litNode

        pure [ (src, dst) | src <- toList mSrc, dst <- litNode : dsts ]
      TSym sym -> pure [ (NConstant (CSym sym), dst) | dst <- dsts ]
      TWild    -> pure [ (NConstant CWild     , dst) | dst <- dsts ]

  pure $ mconcat edgess

--------------------------------------------------------------------------------
-- Monadic actions
--------------------------------------------------------------------------------

newtype SidewaysSt = SidewaysSt { _binderMap :: M.Map Var Node }

type Sideways = ReaderT IS.IntSet (State SidewaysSt)

initSidewaysSt :: SidewaysSt
initSidewaysSt = SidewaysSt M.empty

evalSideways :: IS.IntSet -> Sideways a -> a
evalSideways intentionalIDs = (`evalState` initSidewaysSt)
                            . (`runReaderT` intentionalIDs)

getPredNode :: Int -> Int -> Sideways [ Node ]
getPredNode id ix = do
  intentionalIDs <- ask
  pure [ NPredicate id ix | id `IS.member` intentionalIDs ]

getBinder :: Var -> Sideways (Maybe Node)
getBinder var = lift $ M.lookup var . _binderMap <$> get

updateBinder :: Var -> Node -> Sideways ()
updateBinder var binder = lift $
  modify (\st -> st {_binderMap = M.insert var binder $ _binderMap st})
