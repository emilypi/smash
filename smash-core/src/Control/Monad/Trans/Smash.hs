{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# language Safe #-}
-- |
-- Module       : Control.Monad.Trans.Smash
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : Non-portable
--
-- This module contains utilities for the monad transformer
-- for the smash product.
--
module Control.Monad.Trans.Smash
( -- * Monad transformer
  SmashT(runSmashT)
  -- ** Combinators
, mapSmashT
) where


import Data.Smash

import Control.Applicative (liftA2)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS


-- | A monad transformer for the smash product,
-- parameterized by:
--
--   * @a@ - the value on the left
--   * @b@ - the value on the right
--   * @m@ - The monad over a pointed product (see: 'Smash').
--
newtype SmashT a m b = SmashT { runSmashT :: m (Smash a b) }

-- | Map both the left and right values and output of a computation using
-- the given function.
--
-- * @'runSmashT' ('mapSmashT' f m) = f . 'runSmashT' m@
--
mapSmashT :: (m (Smash a b) -> n (Smash c d)) -> SmashT a m b -> SmashT c n d
mapSmashT f = SmashT . f . runSmashT

instance Functor f => Functor (SmashT a f) where
  fmap f = SmashT . fmap (fmap f) . runSmashT

instance (Monoid a, Applicative f) => Applicative (SmashT a f) where
  pure = SmashT . pure . pure
  SmashT f <*> SmashT a = SmashT $ liftA2 (<*>) f a

instance (Monoid a, Monad m) => Monad (SmashT a m) where
  return = pure

  SmashT m >>= k = SmashT $ do
    c <- m
    case c of
      Smash a b -> do
        c' <- runSmashT $ k b
        return $ case c' of
          Nada -> Nada
          Smash a' b' -> Smash (a <> a') b'
      Nada -> return Nada

instance (Monoid a, MonadReader r m) => MonadReader r (SmashT a m) where
  ask = lift ask
  local f (SmashT m) = SmashT $ local f m

instance (Monoid a, MonadWriter w m) => MonadWriter w (SmashT a m) where
  tell = lift . tell

  listen (SmashT m) = SmashT $ go <$> listen m where
    go (c,w) = case c of
      Nada -> Nada
      Smash a b -> Smash a (b, w)

  pass (SmashT m) = SmashT $ pass (go <$> m) where
    go = \case
      Nada -> (Nada, id)
      Smash t (a, f) -> (Smash t a, f)

instance (Monoid t, MonadState s m) => MonadState s (SmashT t m) where
  get = lift get
  put = lift . put

instance (Monoid t, MonadRWS r w s m) => MonadRWS r w s (SmashT t m)

instance Monoid a => MonadTrans (SmashT a) where
  lift = SmashT . fmap (Smash mempty)
