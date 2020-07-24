{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Control.Monad.ReaderState.Strict
  ( ReaderStateT (runReaderStateT)
  , ReaderState
  , runReaderState
  ) where

import Control.Monad.Error.Class  (MonadError)
import Control.Monad.Reader.Class (MonadReader (ask, local))
import Control.Monad.State.Strict (MonadState, State, StateT, get, put, withStateT)
import Control.Monad.Trans        (MonadTrans)
import Data.Functor.Identity      (Identity)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadCatch, MonadThrow)

newtype ReaderStateT s m a =
  ReaderStateT { runReaderStateT :: StateT s m a }
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadError e, MonadState s, MonadThrow, MonadCatch)

instance Monad m => MonadReader r (ReaderStateT r m) where
  ask = get

  local f m = do
    s <- get
    a <- ReaderStateT $ withStateT f $ runReaderStateT m
    put s
    pure a

type ReaderState s = ReaderStateT s Identity

runReaderState :: ReaderState s a -> State s a
runReaderState = runReaderStateT
