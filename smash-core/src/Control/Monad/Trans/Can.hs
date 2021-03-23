{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Trans.Can
( CanT(runCanT)
, mapCanT
) where


import Data.Can
import Control.Applicative (liftA2)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Except

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

instance (Monoid w, MonadWriter w m) => MonadWriter w (CanT w m) where
  tell a = CanT $ Eno <$> tell a

  listen (CanT m) = CanT $ go <$> listen m where
    go (c,w) = case c of
      Non -> Non
      One a -> One a
      Eno b -> Eno (b,w)
      Two a b -> Two a (b,w)

  pass (CanT m) = CanT (go <$> m) where
    go = \case
      Non -> Non
      One w -> One w
      Eno (a, _) -> Eno a
      Two w (a, ww) -> Two (ww w) a

instance (Semigroup r, MonadReader r m) => MonadReader r (CanT r m) where
  ask = CanT (asks One)
  local f (CanT m) = CanT (go <$> m) where
    go = \case
      Non -> Non
      One r -> One (f r)
      Eno b -> Eno b
      Two r b -> Two (f r) b

instance (Monoid s, MonadState s m) => MonadState s (CanT s m) where
  get = CanT $ One <$> get
  put s = CanT $ Eno <$> put s


instance MonadTrans (CanT a) where
  lift = CanT . fmap Eno

instance (MonadError e m, Semigroup e) => MonadError e (CanT e m) where
  throwError e = CanT $ One <$> throwError e
  catchError (CanT m) f = CanT $ catchError m (runCanT . f)