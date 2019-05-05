{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Dataflow where

import Protolude hiding (head, sym)

-- import Numeric.LinearAlgebra hiding (sym)

-- import           Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import qualified Data.Map.Strict as M
-- import qualified Data.Bimap as BM
import qualified Data.IntSet as IS

import Language.Exalog.Core
import Language.Exalog.Renamer

--------------------------------------------------------------------------------
-- Dataflow graph extraction
--------------------------------------------------------------------------------

data Constant = CSym Sym | CWild deriving (Eq, Ord)

data Node =
    NPredicate { _predicateID :: Int, _paramIndex :: Int }
  | NLiteral   { _literalID   :: Int, _paramIndex :: Int }
  | NConstant  { _constant    :: Constant }
  deriving (Eq, Ord)

type Edge = (Node, Node)

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

handleHeadLiteral :: Literal ('ARename ann) -> Sideways ()
handleHeadLiteral Literal{..} =
  forM_ (zip [0..] $ V.toList terms) $ \case
    (ix, TVar var) -> updateBinder var (NPredicate (uniqID predicate) ix)
    _              -> pure ()

handleBodyLiteral :: Literal ('ARename ann) -> Sideways [ Edge ]
handleBodyLiteral Literal{..} = do
  edgess <- forM (zip [0..] $ V.toList terms) $ \(ix, term) -> do
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

clauseEdges :: IS.IntSet -> Clause ('ARename ann) -> [ Edge ]
clauseEdges intentionals Clause{..} = join . evalSideways intentionals $ do
  handleHeadLiteral head

  traverse handleBodyLiteral (NE.toList body)

programEdges :: Program ('ARename ann) -> [ Edge ]
programEdges pr@Program{..} = concatMap (clauseEdges intentionals) clauses
  where
  intentionals = IS.fromList . map uniqID . findIntentionals $ pr

--------------------------------------------------------------------------------
-- Matrix operations
--------------------------------------------------------------------------------

-- newtype PredNodeCount = PredNodeCount Int
-- type NodeMatrixMap = BM.Bimarp Node Int
-- 
-- fromGraph :: [ Edge ] -> (Matrix I, NodeMatrixMap, PredNodeCount)
-- fromGraph edges = (assoc (len,len) 0 assocList , bmap, predNodeCount)
--   where
--   nodes = nub . sort $ map fst edges ++ map snd edges
--   predNodeCount = PredNodeCount $
--     length (takeWhile (\case {NPredicate{} -> True; _ False})) nodes
--   len = length nodes
-- 
--   bmap = BM.fromList $ zip nodes [0..]
-- 
--   assocList = map ((,1) . bimap (bmap BM.!) (bmap BM.!)) edges
-- 
-- process :: PredNodeCodunt -> Matrix I -> Matrix I
-- process (PredNodeCount pNodeCount) m = step (m * m)
--   where
--   nOfRows = rows m
-- 
--   blah = map []
