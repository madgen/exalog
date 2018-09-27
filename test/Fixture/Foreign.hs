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
  ) where

import Protolude hiding (isPrefixOf)

import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Sized as V
import           Data.Singletons.TypeLits

import           Language.Exalog.Core
import           Language.Exalog.ForeignFunction
import           Language.Exalog.Relation
import qualified Language.Exalog.Tuples as T

import Fixture.Util

srcPred, src2Pred :: Predicate 1 'ABase
srcPred =  Predicate PredABase "src" SNat Logical
src2Pred = Predicate PredABase "src2" SNat Logical

leqPred, isPrefixOfPred :: Predicate 2 'ABase
leqPred = Predicate PredABase "<" SNat (Extralogical $ liftPredicate ((<) :: Int -> Int -> Bool))
isPrefixOfPred = Predicate PredABase "isPrefixOf" SNat (Extralogical $ liftPredicate (Text.isPrefixOf :: Text -> Text -> Bool))

leq100Pred :: Predicate 1 'ABase
leq100Pred = Predicate PredABase "leq100" SNat Logical

prefixOfPred :: Predicate 2 'ABase
prefixOfPred = Predicate PredABase "prefixOf" SNat Logical

src :: Term -> Literal 'ABase
src t = lit srcPred $ fromJust $ V.fromList [ t ]

src2 :: Term -> Literal 'ABase
src2 t = lit src2Pred $ fromJust $ V.fromList [ t ]

leq :: Term -> Term -> Literal 'ABase
leq t t' = lit leqPred $ fromJust $ V.fromList [ t, t' ]

isPrefixOf :: Term -> Term -> Literal 'ABase
isPrefixOf t t' = lit isPrefixOfPred $ fromJust $ V.fromList [ t, t' ]

leq100 :: Term -> Literal 'ABase
leq100 t = lit leq100Pred $ fromJust $ V.fromList [ t ]

prefixOf :: Term -> Term -> Literal 'ABase
prefixOf t t' = lit prefixOfPred $ fromJust $ V.fromList [ t, t' ]

{-
- src("10").
- src("99").
- src("100").
- src("3000").
- leq100(X) :- src(X), X < 100.
-}
programLeq100 :: Program 'ABase
programLeq100 = Program ProgABase
  [ Clause ClABase (leq100 (tvar "X")) $ NE.fromList
    [ src (tvar "X")
    , leq (tvar "X") (tsym (100 :: Int)) ]
  ]

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

{-
- src2("").
- src2("Mis").
- src2("Andrew").
- src2("Mistral").
- src2("Mistral Contrastin").
- prefixOf(X,Y) :- src(X), src(Y), isPrefixOf(X,Y).
-}
programPrefixOf :: Program 'ABase
programPrefixOf = Program ProgABase
  [ Clause ClABase (prefixOf (tvar "X") (tvar "Y")) $ NE.fromList
    [ src2 (tvar "X")
    , src2 (tvar "Y")
    , isPrefixOf (tvar "X") (tvar "Y") ]
  ]

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
