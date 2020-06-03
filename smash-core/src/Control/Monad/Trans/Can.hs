{-# language CPP #-}
{-# language AutoDeriveTypeable #-}
module Control.Monad.Trans.Can where



-- | The parameterizable 'Can' monad, obtained by composing an arbitrary
-- monad with the 'Can' monad (the pointed product).
--
-- Computations are actions that may produce a value or exit.
--
-- The 'return' function yields a computation that produces that
-- value, while @>>=@ sequences two subcomputations, exiting if either
-- computation does.
--
newtype CanT a m b = CanT { _runCanT :: m (Can a b) }


instance (Eq1 m) => Eq1 (CanT m) where
  liftEq eq (CanT t) (CanT u) = liftEq (liftEq eq) t u
  {-# inline liftEq #-}

instance (Ord1 m) => Ord1 (CanT m) where
  liftCompare c (CanT t) (CanT u) = liftCompare (liftCompare c) t u

instance (Read1 m) => Read1 (CanT m) where
  liftReadsPrec p l =
    let p' = liftReadsPrec p l
        l' = liftReadList p l
    in  readsData $ readsUnaryWith (liftReadsPrec p' l') "CanT" CanT

instance (Show1 m) => Show1 (CanT m) where
  liftShowsPrec p l d (CanT m) =
    let p' = liftShowsPrec p l
        l' = liftShowList p l
    in showsUnaryWith (liftShowsPrec p' l') "CanT" d m


instance (Eq1 m, Eq a) => Eq (CanT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (CanT m a) where compare = compare1
instance (Read1 m, Read a) => Read (CanT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (CanT m a) where showsPrec = showsPrec1

-- | Transform the computation inside a @CanT@.
--
-- * @'runCanT' ('mapCanT' f m) = f ('runCanT' m)@
mapCanT :: (m (Can a) -> n (Can b)) -> CanT m a -> CanT n b
mapCanT f = CanT . f . runCanT
{-# inline mapCanT #-}

-- | Convert a 'CanT' computation to 'ExceptT', with a default
-- exception value.
maybeToExceptT :: (Functor m) => e -> CanT m a -> ExceptT e m a
maybeToExceptT e (CanT m) = ExceptT $ fmap (maybe (Left e) Right) m
{-# inline maybeToExceptT #-}

-- | Convert a 'ExceptT' computation to 'CanT', discarding the
-- value of any exception.
exceptToCanT :: (Functor m) => ExceptT e m a -> CanT m a
exceptToCanT (ExceptT m) = CanT $ fmap (either (const Nothing) Just) m
{-# inline exceptToCanT #-}

instance (Functor m) => Functor (CanT m) where
    fmap f = mapCanT (fmap (fmap f))
    {-# inline fmap #-}

instance (Foldable f) => Foldable (CanT f) where
    foldMap f (CanT a) = foldMap (foldMap f) a
    {-# inline foldMap #-}

instance (Traversable f) => Traversable (CanT f) where
    traverse f (CanT a) = CanT <$> traverse (traverse f) a
    {-# inline traverse #-}

instance (Functor m, Monad m) => Applicative (CanT m) where
    pure = CanT . return . Just
    {-# inline pure #-}
    mf <*> mx = CanT $ do
      mb_f <- runCanT mf
      case mb_f of
        Nothing -> return Nothing
        Just f  -> do
          mb_x <- runCanT mx
          case mb_x of
            Nothing -> return Nothing
            Just x  -> return (Just (f x))
    {-# inline (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# inline (*>) #-}

instance (Functor m, Monad m) => Alternative (CanT m) where
    empty = CanT (return Nothing)
    {-# inline empty #-}
    x <|> y = CanT $ do
      v <- runCanT x
      case v of
        Nothing -> runCanT y
          Just _  -> return v
    {-# inline (<|>) #-}

instance (Monad m) => Monad (CanT m) where
    return = CanT . return . Just
    {-# inline return #-}

    x >>= f = CanT $ do
      v <- runCanT x
      case v of
        Nothing -> return Nothing
        Just y  -> runCanT (f y)
    {-# inline (>>=) #-}
#if !(MIN_VERSION_base(4,13,0))
    fail _ = CanT (return Nothing)
    {-# inline fail #-}
#endif


instance (Monad m) => Fail.MonadFail (CanT m) where
    fail _ = CanT (return Nothing)
    {-# inline fail #-}

instance (Monad m) => MonadPlus (CanT m) where
    mzero = CanT (return Nothing)
    {-# inline mzero #-}

    mplus x y = CanT $ do
        v <- runCanT x
        case v of
          Nothing -> runCanT y
          Just _  -> return v
    {-# inline mplus #-}

instance (MonadFix m) => MonadFix (CanT m) where
    mfix f = CanT (mfix (runCanT . f . fromCan e))
      where e = error "mfix (CanT): inner computation returned Non"
    {-# inline mfix #-}

instance MonadTrans CanT where
    lift = CanT . liftM Just
    {-# inline lift #-}

instance (MonadIO m) => MonadIO (CanT m) where
    liftIO = lift . liftIO
    {-# inline liftIO #-}

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (CanT m) where
    contramap f = CanT . contramap (fmap f) . runCanT
    {-# inline contramap #-}
#endif

-- | Lift a @callCC@ operation to the new monad.
--
liftCallCC :: CallCC m (Can a) (Can b) -> CallCC (CanT m) a b
liftCallCC callCC f =
    CanT $ callCC $ \ c -> runCanT (f (CanT . c . Just))
{-# inline liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
--
liftCatch :: Catch e m (Can a) -> Catch e (CanT m) a
liftCatch f m h = CanT $ f (runCanT m) (runCanT . h)
{-# inline liftCatch #-}

-- | Lift a @listen@ operation to the new monad.
--
liftListen :: (Monad m) => Listen w m (Can a) -> Listen w (CanT m) a
liftListen listen = mapCanT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
{-# inline liftListen #-}

-- | Lift a @pass@ operation to the new monad.
--
liftPass :: (Monad m) => Pass w m (Can a) -> Pass w (CanT m) a
liftPass pass = mapCanT $ \ m -> pass $ do
    a <- m
    return $! case a of
      Nothing -> (Nothing, id)
      Just (v, f) -> (Just v, f)
{-# inline liftPass #-}
