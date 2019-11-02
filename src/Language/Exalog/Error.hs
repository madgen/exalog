{-# LANGUAGE RecordWildCards #-}

module Language.Exalog.Error
  ( Error(..)
  , Severity(..)
  ) where

import Protolude hiding ((<>))

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper (Pretty(..))
import Language.Exalog.SrcLoc (SrcSpan, prettySpan)

data Severity =
  -- |Error that should never be thrown
    Impossible
  -- |Standard user error
  | User
  -- |Warning
  | Warning
  deriving (Eq)

data Error = Error
  { _severity :: Severity
  , _mSource  :: Maybe Text
  , _mSpan    :: Maybe SrcSpan
  , _message  :: Text
  }

instance Pretty Severity where
  pretty Impossible = "Impossible happened! Please submit a bug report"
  pretty User       = "Error"
  pretty Warning    = "Warning"

instance Pretty Error where
  pretty Error{..} =
        brackets (pretty _severity) <> colon
    $+$ nest 2 prettyError
    where
    prettyError = pretty _mSpan
              $+$ pretty _message
              $+$ maybe mempty
                        (("" $+$) . uncurry prettySpan)
                        ((,) <$> _mSource <*> _mSpan)
