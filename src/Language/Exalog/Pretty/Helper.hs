module Language.Exalog.Pretty.Helper
  ( pp
  , Pretty(..)
  , PrettyCollection(..)
  , (<?>)
  , (<+?>)
  , cond
  , csep
  ) where

import Protolude hiding ((<>), empty, head)

import           Data.String (fromString)
import           Data.Text (unpack)

import Text.PrettyPrint

-- | Render as text
pp :: Pretty a => a -> Text
pp = fromString . render . pretty

class Pretty a where
  pretty :: a -> Doc

class PrettyCollection a where
  prettyC :: a -> [ Doc ]

infixl 7 <?>
infix  7 <+?>

-- | Same as `<>` but `empty` acts as an annihilator
(<?>) :: Doc -> Doc -> Doc
d1 <?> d2
  | isEmpty d1 || isEmpty d2 = empty
  | otherwise = d1 <> d2

-- | Same as `<+>` but `empty` acts as an annihilator
(<+?>) :: Doc -> Doc -> Doc
d1 <+?> d2
  | isEmpty d1 || isEmpty d2 = empty
  | otherwise = d1 <+> d2

-- | Conditionally return the second argument
cond :: Bool -> Doc -> Doc
cond True doc = doc
cond False _  = empty

-- | Comma separate Docs
csep :: [ Doc ] -> Doc
csep = hsep . punctuate comma

-- Common instances

instance {-# OVERLAPPABLE #-} Pretty a => PrettyCollection [ a ] where
  prettyC = map pretty

instance Pretty Text where
  pretty = text . unpack

instance Pretty () where
  pretty _ = empty

instance Pretty Int where
  pretty = int
