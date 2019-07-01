{-# LANGUAGE DataKinds #-}

module Fixture.RangeRestriction where

import Protolude

import           Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.Relation
import           Language.Exalog.Renamer
import           Language.Exalog.SrcLoc
import qualified Language.Exalog.Tuples as T

import Fixture.Util

pPred, rPred, guard0Pred, queryPred :: Predicate 1 'ABase
pPred      = Predicate (PredABase dummySpan) "p"      SNat Logical
rPred      = Predicate (PredABase dummySpan) "r"      SNat Logical
guard0Pred = Predicate (PredABase dummySpan) "guard0" SNat Logical
queryPred  = Predicate (PredABase dummySpan) "query"  SNat Logical

pPred', rPred', guard0Pred', queryPred' :: Predicate 1 ('ARename 'ABase)
pPred'      = Predicate (PredARename (PredicateID 0) $ PredABase dummySpan) "p"      SNat Logical
rPred'      = Predicate (PredARename (PredicateID 1) $ PredABase dummySpan) "r"      SNat Logical
guard0Pred' = Predicate (PredARename (PredicateID 2) $ PredABase dummySpan) "guard0" SNat Logical
queryPred'  = Predicate (PredARename (PredicateID 3) $ PredABase dummySpan) "query"  SNat Logical

qPred :: Predicate 0 'ABase
qPred = Predicate (PredABase dummySpan) "q" SNat Logical

qPred' :: Predicate 0 ('ARename 'ABase)
qPred' = Predicate (PredARename (PredicateID 4) $ PredABase dummySpan) "q" SNat Logical

p, r, guard0, query :: Term -> Literal 'ABase
p      t = lit pPred      $ fromJust $ V.fromList [ t ]
r      t = lit rPred      $ fromJust $ V.fromList [ t ]
guard0 t = lit guard0Pred $ fromJust $ V.fromList [ t ]
query  t = lit queryPred  $ fromJust $ V.fromList [ t ]

p', r', guard0', query' :: Term -> Literal ('ARename 'ABase)
p'      t = Literal (LitARename (LiteralID 5) $ LitABase dummySpan) Positive pPred' (fromJust $ V.fromList [ t ])
r'      t = Literal (LitARename (LiteralID 6) $ LitABase dummySpan) Positive rPred'  (fromJust $ V.fromList [ t ])
guard0' t = Literal (LitARename (LiteralID 7) $ LitABase dummySpan) Positive guard0Pred' (fromJust $ V.fromList [ t ])
query'  t = Literal (LitARename (LiteralID 8) $ LitABase dummySpan) Positive queryPred' (fromJust $ V.fromList [ t ])

q :: Literal 'ABase
q = lit qPred $ fromJust $ V.fromList [ ]

q' :: Literal ('ARename 'ABase)
q' = Literal (LitARename (LiteralID 9) $ LitABase dummySpan) Positive qPred' (fromJust $ V.fromList [ ])

{-|
- p(X) :- q()
- query(X) :- r(X), p(X)
|-}
prSimple :: Program ('ARename 'ABase)
prSimple = Program (ProgARename $ ProgABase dummySpan)
  [ [ Clause (ClARename (ClauseID 10) $ ClABase dummySpan) (p'     (tvar "X")) $ NE.fromList [ q' ]
    , Clause (ClARename (ClauseID 11) $ ClABase dummySpan) (query' (tvar "X")) $ NE.fromList
      [ r' (tvar "X"), p' (tvar "X") ]
    ]
  ] [ PredicateBox queryPred' ]

{-|
- p(X) :- guard(X), q()
- query(X) :- r(X), p(X)
- guard(X) :- r(X)
|-}
prSimpleRepaired :: Program 'ABase
prSimpleRepaired = Program (ProgABase dummySpan)
  [ [ Clause (ClABase dummySpan) (p (tvar "X")) $NE.fromList
      [ guard0 (tvar "X"), q ]
    , Clause (ClABase dummySpan) (query (tvar "X")) $ NE.fromList
      [ r (tvar "X"), p (tvar "X") ]
    , Clause (ClABase dummySpan) (guard0 (tvar "X")) $ NE.fromList
      [ r (tvar "X") ]
    ]
  ] [ PredicateBox queryPred ]

guard0Tuples :: [ V.Vector 1 Int ]
guard0Tuples = fromJust . V.fromList <$>
  [ [ 1 ] ]

guard0Rel :: Relation 'ABase
guard0Rel = Relation guard0Pred . T.fromList $ fmap symbol <$> guard0Tuples
