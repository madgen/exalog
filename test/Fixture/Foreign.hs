{-# LANGUAGE DataKinds #-}

module Fixture.Foreign
  ( program
  , initEDB
  , leq100Pred
  , leq100Tuples
  ) where

import Protolude

import           Data.Maybe (fromJust)

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.ForeignFunction
import           Language.Exalog.Relation
import qualified Language.Exalog.Tuples as T

import Fixture.Util

srcPred :: Predicate 1 'ABase
srcPred = Predicate PredABase "src" SNat Logical

leqPred :: Predicate 2 'ABase
leqPred = Predicate PredABase "<" SNat (Extralogical $ liftPredicate ((<) :: Int -> Int -> Bool))

leq100Pred :: Predicate 1 'ABase
leq100Pred = Predicate PredABase "leq100" SNat Logical

src :: Term -> Literal 'ABase
src t = lit srcPred $ fromJust $ V.fromList [ t ]

leq :: Term -> Term -> Literal 'ABase
leq t t' = lit leqPred $ fromJust $ V.fromList [ t, t' ]

leq100 :: Term -> Literal 'ABase
leq100 t = lit leq100Pred $ fromJust $ V.fromList [ t ]

{-
- leq100(X) :- src(X), X < 100.
-}
program :: Program 'ABase
program = Program ProgABase
  [ Clause ClABase (leq100 (tvar "X")) $ NE.fromList
    [ src (tvar "X")
    , leq (tvar "X") (tsym "100") ]
  ]

srcTuples :: [ V.Vector 1 Text ]
srcTuples = fromJust . V.fromList <$>
  [ [ "10" ], [ "99" ], [ "100" ], [ "3000" ] ]

srcRel :: Relation 'ABase
srcRel = Relation srcPred . T.fromList $ fmap Sym <$> srcTuples

initEDB :: Solution 'ABase
initEDB = fromList [ srcRel ]

leq100Tuples :: T.Tuples 1
leq100Tuples = T.fromList $ fmap Sym . fromJust . V.fromList <$>
  [ [ "10" ], [ "99" ] ]
