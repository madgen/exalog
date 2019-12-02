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

  -- Impure
  , programImpure
  , initImpureEDB
  , impurePred
  , impureTuples
  ) where

import Protolude hiding (isPrefixOf, Set)

import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.ForeignFunction
import           Language.Exalog.KnowledgeBase.Class
import           Language.Exalog.KnowledgeBase.Knowledge
import           Language.Exalog.KnowledgeBase.Set
import           Language.Exalog.SrcLoc

import Fixture.Util

--------------------------------------------------------------------------------
-- leq100 Fixture
--------------------------------------------------------------------------------

srcPred :: Predicate 1 'ABase
srcPred =  Predicate (PredABase NoSpan) "src" SNat Logical

leqPred :: Predicate 2 'ABase
leqPred = Predicate (PredABase NoSpan) "<" SNat (Extralogical $ liftPredicate ((<) :: Int -> Int -> Bool))

leq100Pred :: Predicate 1 'ABase
leq100Pred = Predicate (PredABase NoSpan) "leq100" SNat Logical

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
programLeq100 = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (leq100 (tvar "X")) $ NE.fromList
        [ src (tvar "X")
        , leq (tvar "X") (tsym (100 :: Int)) ]
      ]
    ])
  [ PredicateBox leq100Pred ]

srcTuples :: [ V.Vector 1 Int ]
srcTuples = fromJust . V.fromList <$>
  [ [ 10 ], [ 99 ], [ 100 ], [ 3000 ] ]

initLeq100EDB :: Set 'ABase
initLeq100EDB = fromList $ mkKnowledge srcPred . fmap symbol <$> srcTuples

leq100Tuples :: [ V.Vector 1 Sym ]
leq100Tuples = fmap symbol . fromJust . V.fromList <$>
  ([ [ 10 ], [ 99 ] ] :: [ [ Int ] ])

--------------------------------------------------------------------------------
-- prefixOf Fixture
--------------------------------------------------------------------------------

src2Pred :: Predicate 1 'ABase
src2Pred = Predicate (PredABase NoSpan) "src2" SNat Logical

isPrefixOfPred :: Predicate 2 'ABase
isPrefixOfPred = Predicate (PredABase NoSpan) "isPrefixOf" SNat (Extralogical $ liftPredicate (Text.isPrefixOf :: Text -> Text -> Bool))

prefixOfPred :: Predicate 2 'ABase
prefixOfPred = Predicate (PredABase NoSpan) "prefixOf" SNat Logical

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
programPrefixOf = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (prefixOf (tvar "X") (tvar "Y")) $ NE.fromList
        [ src2 (tvar "X")
        , src2 (tvar "Y")
        , tvar "X" `isPrefixOf` tvar "Y" ]
      ]
    ])
  [ PredicateBox prefixOfPred ]

src2Tuples :: [ V.Vector 1 Text ]
src2Tuples = fromJust . V.fromList <$>
  ([ [ "" ], [ "Mis" ], [ "Andrew" ], [ "Mistral" ], [ "Mistral Contrastin" ] ]
  :: [ [ Text ] ])

initPrefixOfEDB :: Set 'ABase
initPrefixOfEDB = fromList $ mkKnowledge src2Pred . fmap symbol <$> src2Tuples

prefixOfTuples :: [ V.Vector 2 Sym ]
prefixOfTuples = fmap symbol . fromJust . V.fromList <$>
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
  Predicate (PredABase NoSpan) "cartesian" SNat (Extralogical $ liftFunction cart)

cartesian23Pred :: Predicate 2 'ABase
cartesian23Pred = Predicate (PredABase NoSpan) "cartesian23" SNat Logical

cartesian :: Term -> Term -> Term -> Term -> Literal 'ABase
cartesian t t' t'' t''' = lit cartesianPred $ fromJust $ V.fromList [ t, t', t'', t''' ]

cartesian23 :: Term -> Term -> Literal 'ABase
cartesian23 t t' = lit cartesian23Pred $ fromJust $ V.fromList [ t, t' ]

{-
- cartesian23(X,Y) :- cartesian(2,3,X,Y).
-}
programCartesian23 :: Program 'ABase
programCartesian23 = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (cartesian23 (tvar "X") (tvar "Y")) $ NE.fromList
        [ cartesian (tsym (2 :: Int)) (tsym (3 :: Int)) (tvar "X") (tvar "Y") ]
      ]
    ])
  [ PredicateBox cartesian23Pred ]

initCartesian23EDB :: Set 'ABase
initCartesian23EDB = fromList [ ]

cartesian23Tuples :: [ V.Vector 2 Sym ]
cartesian23Tuples = fmap symbol . fromJust . V.fromList <$>
  ([ [ 1, 1 ] , [ 1, 2 ], [ 1, 3] , [ 2, 1 ] , [ 2, 2 ], [ 2, 3 ] ] :: [ [ Int ] ])

--------------------------------------------------------------------------------
-- Non-pure fixture
--------------------------------------------------------------------------------

impureIDForeign :: Int -> Foreign Int
impureIDForeign = pure

impureIDPred :: Predicate 2 'ABase
impureIDPred = Predicate
  (PredABase NoSpan)
  "impureID"
  SNat
  (Extralogical $ liftFunctionME impureIDForeign)

impureID :: Term -> Term -> Literal 'ABase
impureID t t' = lit impureIDPred $ fromJust $ V.fromList [ t, t' ]

impureFinForeign :: Int -> Foreign [ Int ]
impureFinForeign i = pure [0..i]

impureFinPred :: Predicate 2 'ABase
impureFinPred = Predicate
  (PredABase NoSpan)
  "impureFin"
  SNat
  (Extralogical $ liftFunctionME impureFinForeign)

impureFin :: Term -> Term -> Literal 'ABase
impureFin t t' = lit impureFinPred $ fromJust $ V.fromList [ t, t' ]

impureEvenForeign :: Int -> Foreign Bool
impureEvenForeign = pure . even

impureEvenPred :: Predicate 1 'ABase
impureEvenPred = Predicate
  (PredABase NoSpan)
  "impureEven"
  SNat
  (Extralogical $ liftPredicateME impureEvenForeign)

impureEven :: Term -> Literal 'ABase
impureEven t = lit impureEvenPred $ fromJust $ V.fromList [ t ]

impurePred :: Predicate 1 'ABase
impurePred = Predicate (PredABase NoSpan) "impure" SNat Logical

impure :: Term -> Literal 'ABase
impure t = lit impurePred $ fromJust $ V.fromList [ t ]

programImpure :: Program 'ABase
programImpure = Program (ProgABase NoSpan)
  (Stratum <$>
    [ [ Clause (ClABase NoSpan) (impure (tvar "Y")) $ NE.fromList
        [ impureFin  (tsym (10 :: Int)) (tvar "X")
        , impureEven (tvar "X")
        , impureID   (tvar "X") (tvar "Y")]
      ]
    ])
  [ PredicateBox impurePred ]

initImpureEDB :: Set 'ABase
initImpureEDB = fromList [ ]

impureTuples :: [ V.Vector 1 Sym ]
impureTuples = fmap symbol . fromJust . V.fromList <$>
  ([ [ 0 ], [ 2 ], [ 4 ], [ 6 ], [ 8 ], [ 10 ] ] :: [ [ Int ] ])
