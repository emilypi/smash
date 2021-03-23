{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language Safe #-}
-- |
-- Module       : Control.Monad.Trans.Wedge
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : Non-portable
--
-- This module contains utilities for the monad transformer
-- for the pointed coproduct.
--
module Control.Monad.Trans.Wedge
( -- * Monad transformer
  WedgeT(runWedgeT)
  -- ** Combinators
, mapWedgeT
) where


import Data.Wedge
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
--   * @m@ - The monad over a pointed coproduct (see: 'Wedge').
--
-- This monad transformer is similar to 'ExceptT',
-- except with the possibility of an empty unital value.
--
newtype WedgeT a m b = WedgeT { runWedgeT :: m (Wedge a b) }

-- | Map both the left and right values and output of a computation using
-- the given function.
--
-- * @'runWedgeT' ('mapWedgeT' f m) = f . 'runWedgeT' m@
--
mapWedgeT :: (m (Wedge a b) -> n (Wedge c d)) -> WedgeT a m b -> WedgeT c n d
mapWedgeT f = WedgeT . f . runWedgeT


instance Functor f => Functor (WedgeT a f) where
  fmap f = WedgeT . fmap (fmap f) . runWedgeT

instance (Semigroup a, Applicative f) => Applicative (WedgeT a f) where
  pure = WedgeT . pure . pure
  WedgeT f <*> WedgeT a = WedgeT $ liftA2 (<*>) f a

instance (Semigroup a, Monad m) => Monad (WedgeT a m) where
  return = pure

  WedgeT m >>= k = WedgeT $ do
    c <- m
    case c of
      Nowhere -> return Nowhere
      Here a -> return $ Here a
      There a -> runWedgeT $ k a

instance (MonadReader r m, Semigroup t) => MonadReader r (WedgeT t m) where
  ask = lift ask
  local f (WedgeT m) = WedgeT $ local f m

instance (MonadWriter w m, Semigroup t) => MonadWriter w (WedgeT t m) where
  tell = lift . tell

  listen (WedgeT m) = WedgeT $ go <$> listen m where
    go = \case
      (Nowhere, _) -> Nowhere
      (Here t, _) -> Here t
      (There a, w) -> There (a, w)

  pass (WedgeT m) = WedgeT $ pass (go <$> m) where
    go = \case
     Nowhere -> (Nowhere, id)
     Here w -> (Here w, id)
     There (a,f) -> (There a, f)

instance (MonadState s m, Semigroup t) => MonadState s (WedgeT t m) where
  get = lift get
  put = lift . put

instance (Semigroup t, MonadRWS r w s m) => MonadRWS r w s (WedgeT t m)

instance MonadTrans (WedgeT a) where
  lift = WedgeT . fmap There

instance (MonadError e m, Semigroup e) => MonadError e (WedgeT e m) where
  throwError e = WedgeT $ Here <$> throwError e
  catchError (WedgeT m) f = WedgeT $ catchError m (runWedgeT . f)
