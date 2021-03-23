{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language Safe #-}
-- |
-- Module       : Control.Monad.Trans.Can
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : Non-portable
--
-- This module contains utilities for the monad transformer
-- for the pointed product.
--
module Control.Monad.Trans.Can
( -- * Monad Transformer
  CanT(runCanT)
  -- ** Combinators
, mapCanT
) where


import Data.Can
import Control.Applicative (liftA2)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Except
import Control.Monad.RWS

-- | A monad transformer for the pointed product,
-- parameterized by:
--
--   * @a@ - the value on the left
--   * @b@ - the value on the right
--   * @m@ - The monad over a pointed product (see: 'Can').
--
-- This monad transformer is similar to 'TheseT',
-- except with the possibility of an empty unital value.
--
newtype CanT a m b = CanT { runCanT :: m (Can a b) }

-- | Map both the left and right values and output of a computation using
-- the given function.
--
-- * @'runCanT' ('mapCanT' f m) = f . 'runCanT' m@
--
mapCanT :: (m (Can a b) -> n (Can c d)) -> CanT a m b -> CanT c n d
mapCanT f = CanT . f . runCanT

instance Functor f => Functor (CanT a f) where
  fmap f = CanT . fmap (fmap f) . runCanT

instance (Semigroup a, Applicative f) => Applicative (CanT a f) where
  pure = CanT . pure . pure
  CanT f <*> CanT a = CanT $ liftA2 (<*>) f a

instance (Semigroup a, Monad m) => Monad (CanT a m) where
  return = pure

  CanT m >>= k = CanT $ do
    c <- m
    case c of
      Eno a -> runCanT $ k a
      Two a b -> do
        c' <- runCanT $ k b
        return $ case c' of
          Eno b' -> Two a b'
          Two a' b' -> Two (a <> a') b'
          _ -> c'
      One a -> return $ One a
      Non -> return Non

instance (Semigroup a, MonadWriter w m) => MonadWriter w (CanT a m) where
  tell = lift . tell

  listen (CanT m) = CanT $ go <$> listen m where
    go (c,w) = case c of
      Non -> Non
      One a -> One a
      Eno b -> Eno (b,w)
      Two a b -> Two a (b, w)

  pass (CanT m) = CanT $ pass (go <$> m) where -- collect $200.
    go = \case
      Non -> (Non, id)
      One a -> (One a, id)
      Eno (a,f) -> (Eno a, f)
      Two w (a,f) -> (Two w a, f)


instance (Semigroup a, MonadReader r m) => MonadReader r (CanT a m) where
  ask = lift ask
  local f (CanT m) = CanT (local f m)

instance (MonadState s m, Semigroup t) => MonadState s (CanT t m) where
  get = lift get
  put = lift . put

instance (Semigroup t, MonadRWS r w s m) => MonadRWS r w s (CanT t m)

instance MonadTrans (CanT a) where
  lift = CanT . fmap Eno

instance (MonadError e m, Semigroup e) => MonadError e (CanT e m) where
  throwError = lift . throwError
  catchError (CanT m) f = CanT $ catchError m (runCanT . f)
