{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Data.Smash
( Smash(..)
, smash
, smashFst
, smashSnd
, smashProduct
, smashProduct'
) where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Can
import Data.Data
import Data.Hashable
import Data.Wedge

import GHC.Generics


data Smash a b = Nada | Smash a b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )

smash :: c -> (a -> b -> c) -> Smash a b -> c
smash c _ Nada = c
smash _ f (Smash a b) = f a b

-- | Smash product of two 'Can' datatypes
--
smashProduct :: Can a b -> Smash a b
smashProduct = can Nada (const Nada) (const Nada) Smash

smashProduct' :: a -> b -> Wedge a b -> Smash a b
smashProduct' a b = \case
  Nowhere -> Nada
  Here c -> Smash c b
  There d -> Smash a d

smashFst :: Smash a b -> Maybe a
smashFst Nada = Nothing
smashFst (Smash a _) = Just a

smashSnd :: Smash a b -> Maybe b
smashSnd Nada = Nothing
smashSnd (Smash _ b) = Just b

-- -------------------------------------------------------------------- --
-- Std instances


instance (Hashable a, Hashable b) => Hashable (Smash a b)

instance Functor (Smash a) where
  fmap _ Nada = Nada
  fmap f (Smash a b) = Smash a (f b)

instance Monoid a => Applicative (Smash a) where
  pure = Smash mempty

  Nada <*> _ = Nada
  _ <*> Nada = Nada
  Smash a f <*> Smash c d = Smash (a <> c) (f d)

instance Monoid a => Monad (Smash a) where
  return = pure
  (>>) = (*>)

  Nada >>= _ = Nada
  Smash a b >>= k = case k b of
    Nada -> Nada
    Smash c d -> Smash (a <> c) d

instance (Semigroup a, Semigroup b) => Semigroup (Smash a b) where
  Nada <> b = b
  a <> Nada = a
  Smash a b <> Smash c d = Smash (a <> c) (b <> d)

instance (Semigroup a, Semigroup b) => Monoid (Smash a b) where
  mempty = Nada

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Smash where
  bimap f g = \case
    Nada -> Nada
    Smash a b -> Smash (f a) (g b)

instance Bifoldable Smash where
  bifoldMap f g = \case
    Nada -> mempty
    Smash a b -> f a <> g b

instance Bitraversable Smash where
  bitraverse f g = \case
    Nada -> pure Nada
    Smash a b -> Smash <$> f a <*> g b
