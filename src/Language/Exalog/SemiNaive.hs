{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}


module Language.Exalog.SemiNaive
  ( semiNaive
  , SemiNaive
  , evalSemiNaiveT
  , evalClauses
  ) where

import Protolude hiding (head, pred, sym)

import Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Vector.Sized as V

import           Language.Exalog.Core
import           Language.Exalog.Delta
import           Language.Exalog.Logger
import qualified Language.Exalog.KnowledgeBase.Knowledge as KB
import qualified Language.Exalog.KnowledgeBase.Class as KB
import           Language.Exalog.SrcLoc (Spannable(..))
import qualified Language.Exalog.Unification as U

type SemiNaiveT kb = ReaderT kb
type SemiNaive  kb = SemiNaiveT kb (LoggerT IO)

evalSemiNaiveT :: KB.Knowledgeable kb ann => SemiNaiveT (kb ann) m a -> kb ann -> m a
evalSemiNaiveT = runReaderT

semiNaive :: forall a b c kb
           . SpannableAST a
          => Identifiable (PredicateAnn a) b
          => Identifiable (KnowledgeAnn a) c
          => KB.Knowledgeable kb ('ADelta a)
          => KB.KnowledgeMaker a
          => Semigroup (kb ('ADelta a))
          => Stratum ('ADelta a)
          -> SemiNaive (kb ('ADelta a)) (kb ('ADelta a))
semiNaive stratum@(Stratum clss) = do
  initEDB <- ask

  (`fix` initEDB) $ \f edb -> do
    betterEDB <- local (const edb) step
    if areAllDeltaEmpty betterEDB
      then return betterEDB
      else f betterEDB
  where
  intentionalPreds :: [ PredicateBox a ]
  intentionalPreds = map peel $ intentionals stratum

  areAllDeltaEmpty :: kb ('ADelta a) -> Bool
  areAllDeltaEmpty = KB.null
                   . KB.filter (\(KB.Knowledge _ p _) -> decor p == Delta)

  -- Adds the current deltas to the normal version of the relation.
  -- Basicall S_{i+1} = S_i \cup delta S_i
  updateFromDelta :: KB.KnowledgeMaker a => kb ('ADelta a) -> kb ('ADelta a)
  updateFromDelta edb = 
    sconcat $ edb :| map (updateFromDelta' edb) intentionalPreds

  {-updateFromDelta' :: KB.KnowledgeMaker a => kb ('ADelta a) -> PredicateBox a -> kb ('ADelta a)
  updateFromDelta' edb (PredicateBox p) = KB.fromList $
    KB.mkKnowledge (mkDeltaPredicate Current p) <$> tuples
    where
    deltaTuples = KB.findByPred (mkDeltaPredicate Delta p) edb
    prevTuples  = KB.findByPred (mkDeltaPredicate Prev  p) edb
    tuples = deltaTuples <> prevTuples-}

  updateFromDelta' :: KB.KnowledgeMaker a => kb ('ADelta a) -> PredicateBox a -> kb ('ADelta a)
  updateFromDelta' edb (PredicateBox p) = (`KB.atEach` prevAndDeltaKnowledges) $ \knowledge@(KB.Knowledge ann pred terms) ->
    case decor pred of
      Delta -> KB.Knowledge ann (updateDecor Current pred) terms
      Prev  -> KB.Knowledge ann (updateDecor Current pred) terms
      _       -> knowledge
    where
    prevAndDeltaKnowledges = KB.filter (\KB.Knowledge{_predicate} -> (PredicateBox _predicate) == (PredicateBox (mkDeltaPredicate Delta p)) || (PredicateBox _predicate) == (PredicateBox (mkDeltaPredicate Prev p))) edb  
  
  -- Current to Prev
  shiftPrevs :: (Identifiable (KnowledgeAnn a) id, Ord id) => kb ('ADelta a) -> kb ('ADelta a)
  shiftPrevs kb = (`KB.atEach` kb) $ \knowledge@(KB.Knowledge ann pred terms) ->
    case decor pred of
      Current -> KB.Knowledge ann (updateDecor Prev pred) terms
      _       -> knowledge

  axeDeltaRedundancies :: (Identifiable (KnowledgeAnn a) id, Ord id) => kb ('ADelta a) -> kb ('ADelta a)
  axeDeltaRedundancies edb = (deltas `KB.difference` currentsAsDeltas) <> others
    where
    (deltas,others) =
      KB.partition (\(KB.Knowledge _ pred _) -> decor pred == Delta) edb
    currents =
      KB.filter (\(KB.Knowledge _ pred _) -> decor pred == Current) others
    currentsAsDeltas =
      KB.atEach (\(KB.Knowledge ann pred syms) -> KB.Knowledge ann (updateDecor Delta pred) syms) currents

  step :: (KB.KnowledgeMaker a, Identifiable (KnowledgeAnn a) id, Ord id) => SemiNaive (kb ('ADelta a)) (kb ('ADelta a))
  step = do
    let evalClauses' = evalClauses clss
    let maintenance = updateFromDelta . shiftPrevs . elimDecor Prev
    axeDeltaRedundancies <$> local maintenance evalClauses'

evalClauses :: SpannableAST a
            => Identifiable (PredicateAnn a) b
            => KB.Knowledgeable kb a
            => KB.KnowledgeMaker a
            => Semigroup (kb a)
            => [ Clause a ] -> SemiNaive (kb a) (kb a)
evalClauses clss = do
  kbs <- mapM evalClause clss
  edb <- ask
  return $ sconcat (edb :| kbs)

evalClause :: forall a b kb
            . SpannableAST a
           => Identifiable (PredicateAnn a) b
           => KB.Knowledgeable kb a
           => KB.KnowledgeMaker a
           => Clause a -> SemiNaive (kb a) (kb a)
evalClause cl@Clause{..} = deriveHead =<< foldM walkBody [ U.empty ] _body
  where
  deriveHead :: [ U.Unifier ] -> SemiNaive (kb a) (kb a)
  deriveHead unifiers
    | Literal{_predicate = pred, _terms = terms} <- _head = 
      fmap KB.fromList $ sequence $ do
        unifier <- unifiers
        let preTuple = unifier `U.substitute` terms
        let groundClause = unifier `U.substitute` cl
        let tupleM = lift $ extractHeadTuple preTuple
        pure $ KB.mkKnowledge groundClause pred <$> tupleM

  walkBody :: [ U.Unifier ] -> Literal a -> SemiNaive (kb a) [ U.Unifier ]
  walkBody unifiers lit = fmap (catMaybes . concat) $ sequence $ do
    unifier <- unifiers
    return $ fmap (`U.extend` unifier)
         <$> execLiteral (unifier `U.substitute` lit)

  extractHeadTuple :: V.Vector n Term -> Logger (V.Vector n Sym)
  extractHeadTuple = traverse (\case
    TSym sym -> pure sym
    TVar{}   -> scream (span cl) "Range-restriction is violated"
    TWild    -> scream (span cl) "Head contains a wildcard")

execLiteral :: SpannableAST a
            => Identifiable (PredicateAnn a) b
            => KB.Knowledgeable kb a
            => Literal a -> SemiNaive (kb a) [ U.Unifier ]
execLiteral lit@Literal{_predicate = p@Predicate{_nature = nature}, ..}
  | Extralogical foreignAction <- nature = do
    eTuples <- liftIO $ runExceptT $ foreignAction _terms
    case eTuples of
      Right tuples -> return $ handleTuples _terms tuples
      Left msg -> lift $ scold (span lit) $
        "Fatal foreign function error: " <> msg
  | otherwise = handleTuples _terms . KB.findByPred p <$> ask
  where
  handleTuples :: V.Vector n Term -> [ V.Vector n Sym ] -> [ U.Unifier ]
  handleTuples terms tuples =
    case _polarity of
      Positive -> tuplesToUnifiers terms tuples
      Negative -> [ U.empty | null (tuplesToUnifiers terms tuples) ]

  tuplesToUnifiers :: V.Vector n Term -> [ V.Vector n Sym ] -> [ U.Unifier ]
  tuplesToUnifiers terms = mapMaybe (U.unify terms)
