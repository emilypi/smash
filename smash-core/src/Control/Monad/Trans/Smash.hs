{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
( SmashT(runSmashT)
, mapSmashT
) where


import Data.Smash
import Control.Applicative (liftA2)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State.Class


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

-- instance (Monoid w, MonadWriter w m) => MonadWriter w (SmashT w m) where
--   tell a = SmashT $ (\w -> Smash w ()) <$> tell a

--   listen (SmashT m) = SmashT $ go <$> listen m where
--     go (c,w) = case c of
--       Nada -> Nada
--       Smash a b -> Smash a (b,w)

--   pass (SmashT m) = SmashT (go <$> m) where
--     go = \case
--       Nada -> Nada
--       Smash w (a, ww) -> Smash (ww w) a

-- instance (Monoid r, MonadReader r m) => MonadReader r (SmashT r m) where
--   ask = SmashT $ asks (Smash mempty)
--   local f (SmashT m) = SmashT (go <$> m) where
--     go = \case
--       Nada -> Nada
--       Smash r b -> Smash (f r) b

-- instance (Monoid s, MonadState s m) => MonadState s (SmashT s m) where
--   get = SmashT $ Smash mempty <$> get
--   put s = SmashT $ Smash mempty <$> put s


-- instance MonadTrans (SmashT ()) where
--   lift = SmashT . fmap (Smash ())
