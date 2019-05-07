{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Exalog.Fresh
  ( FreshT
  , Fresh
  , runFreshT
  , runFresh
  , fresh
  ) where

import Protolude

import           Data.Text (pack)
import qualified Data.Set as S

import Control.Monad.Trans.Class (MonadTrans)

data FreshSt = FreshSt
  { _prefix   :: Maybe Text
  , _reserved :: S.Set Text
  , _counter  :: Int
  }

newtype FreshT m a = FreshT (StateT FreshSt m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState FreshSt)
type    Fresh      = FreshT Identity

runFreshT :: Monad m => Maybe Text -> [ Text ] -> FreshT m a -> m a
runFreshT mPrefix reserved (FreshT action) = evalStateT action $ FreshSt
  { _prefix   = mPrefix
  , _reserved = S.fromList reserved
  , _counter  = 0
  }

runFresh :: Maybe Text -> [ Text ] -> Fresh a -> a
runFresh prefix reserved = runIdentity . runFreshT prefix reserved

fresh :: Monad m => FreshT m Text
fresh = do
  st@FreshSt{..} <- get

  put st {_counter = _counter + 1}

  let candidate = fromMaybe "" _prefix <> (pack . show) _counter

  if candidate `elem` _reserved
    then fresh
    else pure candidate
