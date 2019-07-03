{-# LANGUAGE DataKinds #-}

module Fixture.Foreign
  -- Leq100
  ( programLeq100
  , initLeq100EDB
  , leq100Pred
  , leq100Tuples

  -- PrefixOf
  , programPrefixOf
  , initPrefixOfEDB
  , prefixOfPred
  , prefixOfTuples

  -- Cartesian
  , programCartesian23
  , initCartesian23EDB
  , cartesian23Pred
  , cartesian23Tuples
  ) where

import Protolude hiding (isPrefixOf)

import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.ForeignFunction
import qualified Language.Exalog.Tuples as T
import           Language.Exalog.Relation
import           Language.Exalog.SrcLoc

import Fixture.Util

--------------------------------------------------------------------------------
-- leq100 Fixture
--------------------------------------------------------------------------------

srcPred :: Predicate 1 'ABase
srcPred =  Predicate (PredABase dummySpan) "src" SNat Logical

leqPred :: Predicate 2 'ABase
leqPred = Predicate (PredABase dummySpan) "<" SNat (Extralogical $ liftPredicate ((<) :: Int -> Int -> Bool))

leq100Pred :: Predicate 1 'ABase
leq100Pred = Predicate (PredABase dummySpan) "leq100" SNat Logical

src :: Term -> Literal 'ABase
src t = lit srcPred $ fromJust $ V.fromList [ t ]

leq :: Term -> Term -> Literal 'ABase
leq t t' = lit leqPred $ fromJust $ V.fromList [ t, t' ]

leq100 :: Term -> Literal 'ABase
leq100 t = lit leq100Pred $ fromJust $ V.fromList [ t ]

{-
- src("10").
- src("99").
- src("100").
- src("3000").
- leq100(X) :- src(X), X < 100.
-}
programLeq100 :: Program 'ABase
programLeq100 = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (leq100 (tvar "X")) $ NE.fromList
        [ src (tvar "X")
        , leq (tvar "X") (tsym (100 :: Int)) ]
      ]
    ])
  [ PredicateBox leq100Pred ]

srcTuples :: [ V.Vector 1 Int ]
srcTuples = fromJust . V.fromList <$>
  [ [ 10 ], [ 99 ], [ 100 ], [ 3000 ] ]

srcRel :: Relation 'ABase
srcRel = Relation srcPred . T.fromList $ fmap symbol <$> srcTuples

initLeq100EDB :: Solution 'ABase
initLeq100EDB = fromList [ srcRel ]

leq100Tuples :: T.Tuples 1
leq100Tuples = T.fromList $ fmap symbol . fromJust . V.fromList <$>
  ([ [ 10 ], [ 99 ] ] :: [ [ Int ] ])

--------------------------------------------------------------------------------
-- prefixOf Fixture
--------------------------------------------------------------------------------

src2Pred :: Predicate 1 'ABase
src2Pred = Predicate (PredABase dummySpan) "src2" SNat Logical

isPrefixOfPred :: Predicate 2 'ABase
isPrefixOfPred = Predicate (PredABase dummySpan) "isPrefixOf" SNat (Extralogical $ liftPredicate (Text.isPrefixOf :: Text -> Text -> Bool))

prefixOfPred :: Predicate 2 'ABase
prefixOfPred = Predicate (PredABase dummySpan) "prefixOf" SNat Logical

src2 :: Term -> Literal 'ABase
src2 t = lit src2Pred $ fromJust $ V.fromList [ t ]

isPrefixOf :: Term -> Term -> Literal 'ABase
isPrefixOf t t' = lit isPrefixOfPred $ fromJust $ V.fromList [ t, t' ]

prefixOf :: Term -> Term -> Literal 'ABase
prefixOf t t' = lit prefixOfPred $ fromJust $ V.fromList [ t, t' ]

{-
- src2("").
- src2("Mis").
- src2("Andrew").
- src2("Mistral").
- src2("Mistral Contrastin").
- prefixOf(X,Y) :- src(X), src(Y), isPrefixOf(X,Y).
-}
programPrefixOf :: Program 'ABase
programPrefixOf = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (prefixOf (tvar "X") (tvar "Y")) $ NE.fromList
        [ src2 (tvar "X")
        , src2 (tvar "Y")
        , isPrefixOf (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox prefixOfPred ]

src2Tuples :: [ V.Vector 1 Text ]
src2Tuples = fromJust . V.fromList <$>
  ([ [ "" ], [ "Mis" ], [ "Andrew" ], [ "Mistral" ], [ "Mistral Contrastin" ] ]
  :: [ [ Text ] ])

src2Rel :: Relation 'ABase
src2Rel = Relation src2Pred . T.fromList $ fmap symbol <$> src2Tuples

initPrefixOfEDB :: Solution 'ABase
initPrefixOfEDB = fromList [ src2Rel ]

prefixOfTuples :: T.Tuples 2
prefixOfTuples = T.fromList $ fmap symbol . fromJust . V.fromList <$>
  ([ [ "", "" ], [ "", "Mis" ], [ "", "Andrew" ], [ "", "Mistral" ], [ "", "Mistral Contrastin" ]
  , [ "Mis", "Mis" ], [ "Mis", "Mistral" ], [ "Mis", "Mistral Contrastin" ]
  , [ "Andrew", "Andrew" ]
  , [ "Mistral", "Mistral" ], [ "Mistral", "Mistral Contrastin" ]
  , [ "Mistral Contrastin", "Mistral Contrastin" ]
  ] :: [ [ Text ] ])

--------------------------------------------------------------------------------
-- cartesian Fixture
--------------------------------------------------------------------------------

cart :: Int -> Int -> [ (Int, Int) ]
cart n m = [ (i,j) | i <- [1..n], j <- [1..m] ]

cartesianPred :: Predicate 4 'ABase
cartesianPred =
  Predicate (PredABase dummySpan) "cartesian" SNat (Extralogical $ liftFunction cart)

cartesian23Pred :: Predicate 2 'ABase
cartesian23Pred = Predicate (PredABase dummySpan) "cartesian23" SNat Logical

cartesian :: Term -> Term -> Term -> Term -> Literal 'ABase
cartesian t t' t'' t''' = lit cartesianPred $ fromJust $ V.fromList [ t, t', t'', t''' ]

cartesian23 :: Term -> Term -> Literal 'ABase
cartesian23 t t' = lit cartesian23Pred $ fromJust $ V.fromList [ t, t' ]

{-
- cartesian23(X,Y) :- cartesian(2,3,X,Y).
-}
programCartesian23 :: Program 'ABase
programCartesian23 = Program (ProgABase dummySpan)
  (Stratum <$>
    [ [ Clause (ClABase dummySpan) (cartesian23 (tvar "X") (tvar "Y")) $ NE.fromList
        [ cartesian (tsym (2 :: Int)) (tsym (3 :: Int)) (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox cartesian23Pred ]

initCartesian23EDB :: Solution 'ABase
initCartesian23EDB = fromList [ ]

cartesian23Tuples :: T.Tuples 2
cartesian23Tuples = T.fromList $ fmap symbol . fromJust . V.fromList <$>
  ([ [ 1, 1 ] , [ 1, 2 ], [ 1, 3] , [ 2, 1 ] , [ 2, 2 ], [ 2, 3 ] ] :: [ [ Int ] ])
