{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.SrcLoc
  ( SrcLoc(..)
  , SrcSpan(..)
  , dummySpan
  , transSpan
  , listSpan
  , prettySpan
  , Spannable(..)
  ) where

import Protolude hiding ((<>), empty, SrcLoc)

import Data.Text (lines, justifyLeft, pack, unpack)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

data SrcLoc =
    SrcLoc
      { _file :: !FilePath
      , _line :: !Int
      , _col  :: !Int
      }
  | SrcDummy
  deriving (Eq, Ord, Show)

data SrcSpan = SrcSpan SrcLoc SrcLoc deriving (Eq, Ord, Show)

dummySpan :: SrcSpan
dummySpan = SrcSpan SrcDummy SrcDummy

isBefore :: SrcLoc -> SrcLoc -> Bool
isBefore loc@SrcLoc{} loc'@SrcLoc{} =
  _file loc == _file loc' && -- In the same file
  ( _line loc < _line loc' || -- line number of loc precedes loc'
    ( _line loc == _line loc' &&
      _col loc < _col loc')) -- or on the same line but loc at a preceding col.
isBefore _ _ = False

transSpan :: SrcSpan -> SrcSpan -> SrcSpan
transSpan (SrcSpan loc1 loc2) (SrcSpan loc2' loc3) =
  if loc2 `isBefore` loc2'
    then SrcSpan loc1 loc3
    else panic "The first span is not before the second."

listSpan :: [ SrcSpan ] -> SrcSpan
listSpan = foldr transSpan
                 (panic "A span of an empty list of spans is undefined.")

--------------------------------------------------------------------------------
-- Spans of various nodes
--------------------------------------------------------------------------------

class Spannable a where
  span :: a -> SrcSpan

instance {-# OVERLAPPABLE #-} HasField "_span" r SrcSpan => Spannable r where
  span = getField @"_span"

-- |Unsafe
instance {-# OVERLAPPING #-} (Spannable a, Spannable b) => Spannable (a,b) where
  span (a,b) = transSpan (span a) (span b)

-- |Unsafe
instance {-# OVERLAPPING #-} Spannable a => Spannable [ a ] where
  span as = listSpan (map span as)

instance Spannable Void where
  span = absurd

prettySpan :: Text -> SrcSpan -> Doc
prettySpan src (SrcSpan loc1 loc2)
  | loc1 == SrcDummy || loc2 == SrcDummy = mempty
  | otherwise = vcat
    [ "Context:"
    , vcat $ map (uncurry contextLine) contextLines
    , if nOfLines == 1
        then hcat
           $ replicate 6 " "               -- ^ Line number gap
          ++ replicate (_col loc1 - 1) " " -- ^ Up to the beginning of the error
          ++ replicate nOfCols "^"         -- ^ Highlight
        else mempty
    ]
  where
  contents = zip [(1 :: Int)..] . lines $ src
  contextLines = take nOfLines $ drop (_line loc1 - 1) contents

  contextLine ix line =
    (text . unpack . justifyLeft 6 ' ' . pack . show) ix <> (text . unpack) line

  nOfLines = _line loc2 - _line loc1 + 1
  nOfCols  = _col  loc2 - _col  loc1 + 1

--------------------------------------------------------------------------------
-- Pretty instances
--------------------------------------------------------------------------------

instance Pretty SrcLoc where
  pretty SrcLoc{..} = int _line <> colon <> int _col <> " in " <> text _file
  pretty SrcDummy   = empty

-- |This is really ought to be better.
instance Pretty SrcSpan where
  pretty (SrcSpan loc1 loc2) =
    "From " <> pretty loc1 $+$ nest 2 ("to " <?> pretty loc2 <> colon)

instance Pretty (Maybe SrcSpan) where
  pretty Nothing  = empty
  pretty (Just s) = pretty s
