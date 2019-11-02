{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Exalog.Logger
  ( LoggerT
  , Logger
  , LoggerEnv(..)
  , vanillaEnv
  , runLoggerT
  , whisper
  , scold
  , scream
  , Error
  , Severity(..)
  ) where

import Protolude hiding (log)

import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Language.Exalog.Error (Error(..), Severity(..), printError)
import Language.Exalog.SrcLoc (SrcSpan)

newtype LoggerEnv = LoggerEnv
  { _sourceCode :: Maybe BS.ByteString
  }

vanillaEnv :: LoggerEnv
vanillaEnv = LoggerEnv
  { _sourceCode = Nothing
  }

newtype LoggerT m a = LoggerT (ReaderT LoggerEnv (MaybeT m) a)
  deriving (Functor, Applicative, Monad, MonadIO)
type    Logger      = LoggerT IO

runLoggerT :: Monad m => LoggerEnv -> LoggerT m a -> m (Maybe a)
runLoggerT env (LoggerT act) = runMaybeT (runReaderT act env)

whisper :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m ()
whisper mSpan msg =
  liftIO . printError $ Error Warning mSpan msg

scold :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m a
scold mSpan msg = do
  liftIO . printError $ Error User mSpan msg
  LoggerT (lift $ MaybeT (pure Nothing))

scream :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m a
scream mSpan msg = do
  liftIO . printError $ Error Impossible mSpan msg
  LoggerT (lift $ MaybeT (pure Nothing))
