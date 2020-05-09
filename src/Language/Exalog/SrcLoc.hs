{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Exalog.SrcLoc
  ( SrcLoc(..)
  , InputSource(..)
  , SrcSpan(..)
  , transSpan
  , listSpan
  , prettySpan
  , Spannable(..)
  ) where

import Protolude hiding ((<>), empty, SrcLoc)

import Data.Text (justifyLeft, pack, unpack)

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper

data SrcLoc =
    SrcLoc
      { _line :: !Int
      , _col  :: !Int
      }
  deriving (Eq, Ord, Show)

data InputSource = File !FilePath | Stdin | None deriving (Eq, Ord, Show)

data SrcSpan =
    Span !InputSource !SrcLoc !SrcLoc
  | NoSpan
  deriving (Eq, Ord, Show)

isBefore :: SrcLoc -> SrcLoc -> Bool
isBefore loc@SrcLoc{} loc'@SrcLoc{} =
  _line loc < _line loc' ||
  (_line loc == _line loc' && _col loc < _col loc')

transSpan :: SrcSpan -> SrcSpan -> SrcSpan
transSpan NoSpan sp   = sp
transSpan sp   NoSpan = sp
transSpan (Span file1 loc1 loc2) (Span file2 loc2' loc3)
  | file1 /= file2 = panic "Trying to compute transitive span of two different files."
  | loc2 `isBefore` loc2' = Span file1 loc1 loc3
  | otherwise = panic "The first span is not before the second."

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
prettySpan _ NoSpan = mempty
prettySpan src (Span _ loc1 loc2) = vcat
  [ "Context:"
  , vcat $ map (uncurry contextLine) contextLines
  , if nOfLines == 1
      then hcat
         $ replicate 6 " "               -- Line number gap
        ++ replicate (_col loc1 - 1) " " -- Up to the beginning of the error
        ++ replicate nOfCols "^"         -- Highlight
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
  pretty SrcLoc{..} = int _line <> colon <> int _col

-- |This is really ought to be better.
instance Pretty SrcSpan where
  pretty (Span file loc1 loc2) =
    (pretty file <?> colon) <+> pretty loc1 <> "-" <> pretty loc2
  pretty NoSpan = mempty

instance Pretty (Maybe SrcSpan) where
  pretty Nothing  = empty
  pretty (Just s) = pretty s

instance Pretty InputSource where
  pretty (File file) = text file
  pretty Stdin       = "STDIN"
  pretty None        = mempty
