{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( AtomicStateMonad (..)
    , IORefMonad
    , runIORefMonad
    ) where

import Control.Monad.IO.Unlift
import Data.IORef


newtype RIO state a = RIO (state -> IO a)

instance Functor (RIO state) where
  f `fmap` RIO r = RIO $ fmap f . r

instance Applicative (RIO state) where
  pure = RIO . const . pure
  RIO r1 <*> RIO r2 = RIO $ \st -> r1 st <*> r2 st

instance Monad (RIO state) where
  RIO r1 >>= f = RIO $ \st -> r1 st >>= (unRIO st . f)
    where
      unRIO st (RIO r) = r st

instance MonadIO (RIO state) where
  liftIO = RIO . const

instance MonadUnliftIO (RIO state) where
  askUnliftIO = RIO $ \st -> pure $ UnliftIO $ \(RIO r) -> r st


class MonadUnliftIO m => AtomicStateMonad state m where
  modify :: (state -> state) -> m ()
  get :: m state


newtype IORefMonad state a = IRM (RIO (IORef state) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

runIORefMonad :: state -> IORefMonad state a -> IO a
runIORefMonad start (IRM (RIO r)) = newIORef start >>= r

instance state1 ~ state2 => AtomicStateMonad state1 (IORefMonad state2) where
  modify f = IRM $ RIO (`atomicModifyIORef'` \st -> (f st, ()))
  get = IRM $ RIO readIORef
