{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Dataflow where

import Protolude hiding (head, sym)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import qualified Data.Map.Strict as M
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

whenIntentional :: Int -> a -> Sideways (Maybe a)
whenIntentional id a = do
  intentionalIDs <- ask
  pure $ if id `IS.member` intentionalIDs
    then Just a
    else Nothing

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
  edgess <- forM (zip [0..] $ V.toList terms) $ \case
    (ix, term) -> do
      let litNode  = NLiteral   (uniqID annotation) ix

      -- Vertical dataflow edge
      let predID = uniqID predicate
      mVerticalEdge <- whenIntentional predID $
        let predNode = NPredicate (uniqID predicate) ix
        in (litNode, predNode)

      -- Horizontal dataflow edge
      mHorizontalEdge <- case term of
        TVar var -> do
          mSrcNode <- getBinder var

          updateBinder var litNode

          pure $ (,litNode) <$> mSrcNode
        TSym sym -> pure $ Just (NConstant (CSym sym), litNode)
        TWild    -> pure $ Just (NConstant CWild     , litNode)

      pure $ case (mVerticalEdge, mHorizontalEdge) of
        (Just vEdge, Just hEdge) -> [ vEdge, hEdge ]
        (Just vEdge, Nothing)    -> [ vEdge ]
        (Nothing   , Just hEdge) -> [ hEdge ]
        (Nothing   , Nothing)    -> []

  pure $ mconcat edgess

clauseEdges :: IS.IntSet -> Clause ('ARename ann) -> [ Edge ]
clauseEdges intentionals Clause{..} = join . evalSideways intentionals $ do
  handleHeadLiteral head

  traverse handleBodyLiteral (NE.toList body)

programEdges :: Program ('ARename ann) -> [ Edge ]
programEdges pr@Program{..} = concatMap (clauseEdges intentionals) clauses
  where
  intentionals = IS.fromList . map uniqID . findIntentionals $ pr
