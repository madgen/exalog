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
  , Err.Error
  , Err.Severity(..)
  ) where

import Protolude hiding (log)

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Language.Exalog.Pretty (pp)
import qualified Language.Exalog.Error as Err
import           Language.Exalog.SrcLoc (SrcSpan)

newtype LoggerEnv = LoggerEnv
  { -- |Optional because test cases don't have source code
    _mSource :: Maybe Text
  }

vanillaEnv :: LoggerEnv
vanillaEnv = LoggerEnv
  { _mSource = Nothing
  }

newtype LoggerT m a = LoggerT (ReaderT LoggerEnv (MaybeT m) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LoggerEnv)
type Logger = LoggerT IO

instance MonadTrans LoggerT where
  lift m = LoggerT (lift (lift m))

runLoggerT :: Monad m => LoggerEnv -> LoggerT m a -> m (Maybe a)
runLoggerT env (LoggerT act) = runMaybeT (runReaderT act env)

whisper :: MonadIO m => SrcSpan -> Text -> LoggerT m ()
whisper = common Err.Warning

scold :: MonadIO m => SrcSpan -> Text -> LoggerT m a
scold mSpan msg = do
  common Err.User mSpan msg
  LoggerT (lift $ MaybeT (pure Nothing))

scream :: MonadIO m => SrcSpan -> Text -> LoggerT m a
scream mSpan msg = do
  common Err.Impossible mSpan msg
  LoggerT (lift $ MaybeT (pure Nothing))

common :: MonadIO m => Err.Severity -> SrcSpan -> Text -> LoggerT m ()
common severity mSpan msg = do
  mSrc <- _mSource <$> ask
  let renderedErr = pp $ Err.Error severity mSrc mSpan msg
  liftIO $ putStrLn renderedErr
