{-# LANGUAGE DataKinds #-}

module Fixture.RangeRestriction
  ( program1
  , program1Repaired
  ) where

import Protolude

import           Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.Relation
import           Language.Exalog.SrcLoc
import qualified Language.Exalog.Tuples as T

import Fixture.Util

pPred, rPred, guard0Pred, queryPred :: Predicate 1 'ABase
pPred      = Predicate (PredABase dummySpan) "p"      SNat Logical
rPred      = Predicate (PredABase dummySpan) "r"      SNat Logical
guard0Pred = Predicate (PredABase dummySpan) "guard0" SNat Logical
queryPred  = Predicate (PredABase dummySpan) "query"  SNat Logical

qPred :: Predicate 0 'ABase
qPred = Predicate (PredABase dummySpan) "q" SNat Logical

p, r, guard0, query :: Term -> Literal 'ABase
p      t = lit pPred      $ fromJust $ V.fromList [ t ]
r      t = lit rPred      $ fromJust $ V.fromList [ t ]
guard0 t = lit guard0Pred $ fromJust $ V.fromList [ t ]
query  t = lit queryPred  $ fromJust $ V.fromList [ t ]

q :: Literal 'ABase
q = lit qPred $ fromJust $ V.fromList [ ]

{-| Linear ancestor program:
-
- p(X) :- q()
- query(X) :- r(X), p(X)
|-}
program1 :: (Program 'ABase, Solution 'ABase)
program1 =
  ( Program (ProgABase dummySpan)
    [ Clause (ClABase dummySpan) (p (tvar "X")) $ NE.fromList [ q ]
    , Clause (ClABase dummySpan) (query (tvar "X")) $ NE.fromList
      [ r (tvar "X"), p (tvar "X") ]
    ] [ PredicateBox queryPred ]
  , fromList [ ]
  )

program1Repaired :: (Program 'ABase, Solution 'ABase)
program1Repaired =
  ( Program (ProgABase dummySpan)
    [ Clause (ClABase dummySpan) (p (tvar "X")) $NE.fromList
      [ guard0 (tvar "X"), q ]
    , Clause (ClABase dummySpan) (query (tvar "X")) $ NE.fromList
      [ r (tvar "X"), p (tvar "X") ]
    , Clause (ClABase dummySpan) (guard0 (tvar "X")) $ NE.fromList
      [ r (tvar "X") ]
    ] [ PredicateBox queryPred ]
  , fromList [ ]
  )

guard0Tuples :: [ V.Vector 1 Int ]
guard0Tuples = fromJust . V.fromList <$>
  [ [ 1 ] ]

guard0Rel :: Relation 'ABase
guard0Rel = Relation guard0Pred . T.fromList $ fmap symbol <$> guard0Tuples
