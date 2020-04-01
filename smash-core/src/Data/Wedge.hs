{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Data.Wedge
( -- * Datatypes
  Wedge(..)
  -- * Combinators
, quotWedge
, wedgeLeft
, wedgeRight
  -- ** Eliminators
, wedge
  -- ** Distributivity
, distributeWedge
, codistributeWedge
  -- ** Associativity
, reassocLR
, reassocRL
  -- ** Symmetry
, swapWedge
) where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import Data.Hashable

import GHC.Generics


data Wedge a b = Nowhere | Here a | There b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )

-- -------------------------------------------------------------------- --
-- Combinators

-- | Take the quotient of two pointed types and produce a wedge
--
quotWedge :: Either (Maybe a) (Maybe b) -> Wedge a b
quotWedge (Left a) = maybe Nowhere Here a
quotWedge (Right b) = maybe Nowhere There b

wedgeLeft :: Maybe a -> Wedge a b
wedgeLeft Nothing = Nowhere
wedgeLeft (Just a) = Here a

wedgeRight :: Maybe b -> Wedge a b
wedgeRight Nothing = Nowhere
wedgeRight (Just b) = There b

-- -------------------------------------------------------------------- --
-- Eliminators

wedge
    :: c
    -> (a -> c)
    -> (b -> c)
    -> Wedge a b
    -> c
wedge c _ _ Nowhere = c
wedge _ f _ (Here a) = f a
wedge _ _ g (There b) = g b

-- -------------------------------------------------------------------- --
-- Eliminators

reassocLR :: Wedge (Wedge a b) c -> Wedge a (Wedge b c)
reassocLR = \case
    Nowhere -> Nowhere
    Here w -> case w of
      Nowhere -> There Nowhere
      Here a -> Here a
      There b -> There (Here b)
    There c -> There (There c)

reassocRL :: Wedge a (Wedge b c) -> Wedge (Wedge a b) c
reassocRL = \case
  Nowhere -> Nowhere
  Here a -> Here (Here a)
  There w -> case w of
    Nowhere -> Here Nowhere
    Here b -> Here (There b)
    There c -> There c

-- -------------------------------------------------------------------- --
-- Distributivity

distributeWedge :: Wedge (Either a b) c -> Wedge a (Wedge b c)
distributeWedge = \case
  Nowhere -> Nowhere
  Here (Left a) -> Here a
  Here (Right b) -> There (Here b)
  There c -> There (There c)

codistributeWedge :: Either (Wedge a c) (Wedge b c) -> Wedge (Either a b) c
codistributeWedge = \case
  Left w -> case w of
    Nowhere -> Nowhere
    Here a -> Here (Left a)
    There c -> There c
  Right w -> case w of
    Nowhere -> Nowhere
    Here b -> Here (Right b)
    There c -> There c

-- -------------------------------------------------------------------- --
-- Symmetry

swapWedge :: Wedge a b -> Wedge b a
swapWedge = \case
  Nowhere -> Nowhere
  Here a -> There a
  There b -> Here b

-- -------------------------------------------------------------------- --
-- Std instances

instance (Hashable a, Hashable b) => Hashable (Wedge a b)


instance Functor (Wedge a) where
  fmap f = \case
    Nowhere -> Nowhere
    Here a -> Here a
    There b -> There (f b)

instance Foldable (Wedge a) where
  foldMap f (There b) = f b
  foldMap _ _ = mempty

instance Traversable (Wedge a) where
  traverse f = \case
    Nowhere -> pure Nowhere
    Here a -> pure (Here a)
    There b -> There <$> f b

instance Semigroup a => Applicative (Wedge a) where
  pure = There

  _ <*> Nowhere = Nowhere
  Nowhere <*> _ = Nowhere
  Here a <*> _ = Here a
  There _ <*> Here b = Here b
  There f <*> There a = There (f a)

instance Semigroup a => Monad (Wedge a) where
  return = pure
  (>>) = (*>)

  Nowhere >>= _ = Nowhere
  Here a >>= _ = Here a
  There b >>= k = k b

instance (Semigroup a, Semigroup b) => Semigroup (Wedge a b) where
  Nowhere <> b = b
  a <> Nowhere = a
  Here a <> Here b = Here (a <> b)
  Here _ <> There b = There b
  There a <> Here _ = There a
  There a <> There b = There (a <> b)

instance (Semigroup a, Semigroup b) => Monoid (Wedge a b) where
  mempty = Nowhere

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Wedge where
  bimap f g = \case
    Nowhere -> Nowhere
    Here a -> Here (f a)
    There b -> There (g b)

instance Bifoldable Wedge where
  bifoldMap f g = \case
    Nowhere -> mempty
    Here a -> f a
    There b -> g b

instance Bitraversable Wedge where
  bitraverse f g = \case
    Nowhere -> pure Nowhere
    Here a -> Here <$> f a
    There b -> There <$> g b
