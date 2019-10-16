{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Exalog.Logger
  ( LoggerT
  , Logger
  , runLoggerT
  , whisper
  , scold
  , scream
  , Error
  , Severity(..)
  ) where

import Protolude hiding (log)

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Language.Exalog.Error (Error(..), Severity(..), printError)
import Language.Exalog.SrcLoc (SrcSpan)

newtype LoggerT m a = LoggerT (MaybeT m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
type    Logger      = LoggerT IO

runLoggerT :: Monad m => LoggerT m a -> m (Maybe a)
runLoggerT (LoggerT maybeT) = runMaybeT maybeT

whisper :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m ()
whisper mSpan msg =
  liftIO . printError $ Error Warning mSpan msg

scold :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m a
scold mSpan msg = do
  liftIO . printError $ Error User mSpan msg
  LoggerT (MaybeT (pure Nothing))

scream :: MonadIO m => Maybe SrcSpan -> Text -> LoggerT m a
scream mSpan msg = do
  liftIO . printError $ Error Impossible mSpan msg
  LoggerT (MaybeT (pure Nothing))
