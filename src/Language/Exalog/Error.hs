module Language.Exalog.Error
  ( Error(..)
  , Severity(..)
  , printError
  ) where

import Protolude hiding ((<>))

import Text.PrettyPrint

import Language.Exalog.Pretty.Helper (Pretty(..), pp)
import Language.Exalog.SrcLoc (SrcSpan, printSpan)

data Severity =
    Impossible -- |Error that should never be thrown
  | User       -- |Standard user error
  | Warning    -- |Warning
  deriving (Eq)

data Error = Error
  { _severity :: Severity
  , _mSpan    :: Maybe SrcSpan
  , _message  :: Text
  }

printError :: MonadIO m => Error -> m ()
printError err = do
  liftIO . putStrLn . pp $ err
  putStrLn ("" :: Text)
  forM_ (_mSpan err) printSpan

instance Pretty Severity where
  pretty Impossible = "Impossible happened! Please submit a bug report"
  pretty User       = "Error"
  pretty Warning    = "Warning"

instance Pretty Error where
  pretty (Error severity mSpan msg) =
    brackets (pretty severity) <> colon $+$
      nest 2 (pretty mSpan $+$ pretty msg)
