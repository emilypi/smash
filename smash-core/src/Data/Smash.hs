{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.Smash
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : CPP, RankNTypes, TypeApplications
--
-- This module contains the definition for the 'Smash' datatype. In
-- practice, this type is isomorphic to 'Maybe (a,b)' - the type with
-- two possibly non-exclusive values and an empty case.
module Data.Smash
( -- * Datatypes
  -- $general
  Smash(..)
  -- * Combinators
, toSmash
, fromSmash
, smashFst
, smashSnd
, quotSmash
, hulkSmash
, isSmash
, isNada
  -- ** Eliminators
, smash
  -- * Filtering
, smashes
, filterNadas
  -- * Folding
, foldSmashes
, gatherSmashes
  -- * Partitioning
, partitionSmashes
, mapSmashes
  -- * Currying & Uncurrying
, smashCurry
, smashUncurry
  -- * Distributivity
, distributeSmash
, undistributeSmash
, pairSmash
, unpairSmash
, pairSmashCan
, unpairSmashCan
  -- * Associativity
, reassocLR
, reassocRL
  -- * Symmetry
, swapSmash
) where


import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData(..))

import Data.Biapplicative
import Data.Bifoldable
import Data.Binary (Binary(..))
import Data.Bitraversable
import Data.Can (Can(..), can)
import Data.Data
import Data.Hashable
import Data.Wedge (Wedge(..))
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif


import GHC.Generics

import Data.Smash.Internal

{- $general

Categorically, the 'Smash' datatype represents a special type of product, a
<https://ncatlab.org/nlab/show/smash+product smash product>, in the category Hask*
of pointed Hask types. The category Hask* consists of Hask types affixed with
a dedicated base point - i.e. all objects look like 'Maybe a'. The smash product is a symmetric, monoidal tensor in Hask* that plays
nicely with the product, 'Can', and coproduct, 'Wedge'. Pictorially,
these datatypes look like this:

@
'Can':
        a
        |
Non +---+---+ (a,b)
        |
        b

'Wedge':
                a
                |
Nowhere +-------+
                |
                b


'Smash':


Nada +--------+ (a,b)
@


The fact that smash products form a closed, symmetric monoidal tensor for Hask*
means that we can speak in terms of the language of linear logic for this category.
Namely, we can understand how 'Smash', 'Wedge', and 'Can' interact. 'Can' and 'Wedge'
distribute nicely over each other, and 'Smash' distributes well over 'Wedge', but
is only semi-distributable over 'Wedge''s linear counterpart, which is left
out of the api. In this library, we focus on the fragment of this pointed linear logic
that makes sense to use, and that will be useful to us as Haskell developers.

-}

-- | The 'Smash' data type represents A value which has either an
-- empty case, or two values. The result is a type, 'Smash a b', which is
-- isomorphic to 'Maybe (a,b)'.
--
-- Categorically, the smash product (the quotient of a pointed product by
-- a wedge sum) has interesting properties. It forms a closed
-- symmetric-monoidal tensor in the category Hask* of pointed haskell
-- types (i.e. 'Maybe' values).
--
data Smash a b = Nada | Smash a b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )

-- -------------------------------------------------------------------- --
-- Combinators

-- | Convert a 'Maybe' value into a 'Smash' value
--
toSmash :: Maybe (a,b) -> Smash a b
toSmash Nothing = Nada
toSmash (Just (a,b)) = Smash a b

-- | Convert a 'Smash' value into a 'Maybe' value
--
fromSmash :: Smash a b -> Maybe (a,b)
fromSmash Nada = Nothing
fromSmash (Smash a b) = Just (a,b)

-- | Smash product of pointed type modulo its wedge
--
quotSmash :: Can a b -> Smash a b
quotSmash = can Nada (const Nada) (const Nada) Smash

-- | Take the smash product of a wedge and two default values
-- to place in either the left or right side of the final product
--
hulkSmash :: a -> b -> Wedge a b -> Smash a b
hulkSmash a b = \case
  Nowhere -> Nada
  Here c -> Smash c b
  There d -> Smash a d

-- | Project the left value of a 'Smash' datatype. This is analogous
-- to 'fst' for '(,)'.
--
smashFst :: Smash a b -> Maybe a
smashFst Nada = Nothing
smashFst (Smash a _) = Just a

-- | Project the right value of a 'Smash' datatype. This is analogous
-- to 'snd' for '(,)'.
--
smashSnd :: Smash a b -> Maybe b
smashSnd Nada = Nothing
smashSnd (Smash _ b) = Just b

-- | Detect whether a 'Smash' value is empty
--
isNada :: Smash a b -> Bool
isNada Nada = True
isNada _ = False

-- | Detect whether a 'Smash' value is not empty
--
isSmash :: Smash a b -> Bool
isSmash = not . isNada

-- -------------------------------------------------------------------- --
-- Eliminators

-- | Case elimination for the 'Smash' datatype
--
smash :: c -> (a -> b -> c) -> Smash a b -> c
smash c _ Nada = c
smash _ f (Smash a b) = f a b

-- -------------------------------------------------------------------- --
-- Filtering

-- | Given a 'Foldable' of 'Smash's, collect the values of the
-- 'Smash' cases, if any.
--
smashes :: Foldable f => f (Smash a b) -> [(a,b)]
smashes = foldr go []
  where
    go (Smash a b) acc = (a,b) : acc
    go _ acc = acc

-- | Filter the 'Nada' cases of a 'Foldable' of 'Smash' values.
--
filterNadas :: Foldable f => f (Smash a b) -> [Smash a b]
filterNadas = foldr go []
  where
    go Nada acc = acc
    go a acc = a:acc

-- -------------------------------------------------------------------- --
-- Folding

-- | Fold over the 'Smash' case of a 'Foldable' of 'Smash' products by
-- some accumulatig function.
--
foldSmashes
    :: Foldable f
    => (a -> b -> m -> m)
    -> m
    -> f (Smash a b)
    -> m
foldSmashes f = foldr go
  where
    go (Smash a b) acc = f a b acc
    go _ acc = acc

-- | Gather a 'Smash' product of two lists and product a list of 'Smash'
-- values, mapping the 'Nada' case to the empty list and zipping
-- the two lists together with the 'Smash' constructor otherwise.
--
gatherSmashes :: Smash [a] [b] -> [Smash a b]
gatherSmashes (Smash as bs) = zipWith Smash as bs
gatherSmashes _ = []

-- -------------------------------------------------------------------- --
-- Partitioning

-- | Given a 'Foldable' of 'Smash's, partition it into a tuple of alternatives
-- their parts.
--
partitionSmashes
    :: forall f t a b
    . ( Foldable t
      , Alternative f
      )
    => t (Smash a b) -> (f a, f b)
partitionSmashes = foldr go (empty, empty)
  where
    go Nada acc = acc
    go (Smash a b) (as, bs) = (pure a <|> as, pure b <|> bs)

-- | Partition a structure by mapping its contents into 'Smash's,
-- and folding over '(<|>)'.
--
mapSmashes
    :: forall f t a b c
    . ( Alternative f
      , Traversable t
      )
    => (a -> Smash b c)
    -> t a
    -> (f b, f c)
mapSmashes f = partitionSmashes . fmap f

-- -------------------------------------------------------------------- --
-- Currying & Uncurrying

-- | "Curry" a map from a smash product to a pointed type. This is analogous
-- to 'curry' for '(->)'.
--
smashCurry :: (Smash a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
smashCurry f (Just a) (Just b) = f (Smash a b)
smashCurry _ _ _ = Nothing

-- | "Uncurry" a map of pointed types to a map of a smash product to a pointed type.
-- This is analogous to 'uncurry' for '(->)'.
--
smashUncurry :: (Maybe a -> Maybe b -> Maybe c) -> Smash a b -> Maybe c
smashUncurry _ Nada = Nothing
smashUncurry f (Smash a b) = f (Just a) (Just b)

-- -------------------------------------------------------------------- --
-- Distributivity


-- | A smash product of wedges is a wedge of smash products.
-- Smash products distribute over coproducts ('Wedge's) in pointed Hask
--
distributeSmash ::  Smash (Wedge a b) c -> Wedge (Smash a c) (Smash b c)
distributeSmash (Smash (Here a) c) = Here (Smash a c)
distributeSmash (Smash (There b) c) = There (Smash b c)
distributeSmash _ = Nowhere

-- | A wedge of smash products is a smash product of wedges.
-- Smash products distribute over coproducts ('Wedge's) in pointed Hask
--
undistributeSmash :: Wedge (Smash a c) (Smash b c) -> Smash (Wedge a b) c
undistributeSmash (Here (Smash a c)) = Smash (Here a) c
undistributeSmash (There (Smash b c)) = Smash (There b) c
undistributeSmash _ = Nada

-- | Distribute a 'Smash' of a pair into a pair of 'Smash's
--
pairSmash :: Smash (a,b) c -> (Smash a c, Smash b c)
pairSmash = unzipFirst

-- | Distribute a 'Smash' of a pair into a pair of 'Smash's
--
unpairSmash :: (Smash a c, Smash b c) -> Smash (a,b) c
unpairSmash (Smash a c, Smash b _) = Smash (a,b) c
unpairSmash _ = Nada

-- | Distribute a 'Smash' of a 'Can' into a 'Can' of 'Smash's
--
pairSmashCan :: Smash (Can a b) c -> Can (Smash a c) (Smash b c)
pairSmashCan Nada = Non
pairSmashCan (Smash cc c) = case cc of
  Non -> Non
  One a -> One (Smash a c)
  Eno b -> Eno (Smash b c)
  Two a b -> Two (Smash a c) (Smash b c)

-- | Undistribute a 'Can' of 'Smash's into a 'Smash' of 'Can's.
--
unpairSmashCan :: Can (Smash a c) (Smash b c) -> Smash (Can a b) c
unpairSmashCan cc = case cc of
  One (Smash a c) -> Smash (One a) c
  Eno (Smash b c) -> Smash (Eno b) c
  Two (Smash a c) (Smash b _) -> Smash (Two a b) c
  _ -> Nada

-- -------------------------------------------------------------------- --
-- Associativity

-- | Reassociate a 'Smash' product from left to right.
--
reassocLR :: Smash (Smash a b) c -> Smash a (Smash b c)
reassocLR (Smash (Smash a b) c) = Smash a (Smash b c)
reassocLR _ = Nada

-- | Reassociate a 'Smash' product from right to left.
--
reassocRL :: Smash a (Smash b c) -> Smash (Smash a b) c
reassocRL (Smash a (Smash b c)) = Smash (Smash a b) c
reassocRL _ = Nada

-- -------------------------------------------------------------------- --
-- Symmetry

-- | Swap the positions of values in a 'Smash a b' to form a 'Smash b a'.
--
swapSmash :: Smash a b -> Smash b a
swapSmash Nada = Nada
swapSmash (Smash a b) = Smash b a

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
  Smash a f <*> Smash c d = Smash (a `mappend` c) (f d)

instance Monoid a => Monad (Smash a) where
  return = pure
  (>>) = (*>)

  Nada >>= _ = Nada
  Smash a b >>= k = case k b of
    Nada -> Nada
    Smash c d -> Smash (a `mappend` c) d

instance (Semigroup a, Semigroup b) => Semigroup (Smash a b) where
  Nada <> b = b
  a <> Nada = a
  Smash a b <> Smash c d = Smash (a <> c) (b <> d)

instance (Semigroup a, Semigroup b) => Monoid (Smash a b) where
  mempty = Nada
  mappend = (<>)

instance (NFData a, NFData b) => NFData (Smash a b) where
  rnf Nada = ()
  rnf (Smash a b) = rnf a `seq` rnf b

instance (Binary a, Binary b) => Binary (Smash a b) where
  put Nada = put @Int 0
  put (Smash a b) = put @Int 1 >> put a >> put b

  get = get @Int >>= \case
    0 -> pure Nada
    1 -> Smash <$> get <*> get
    _ -> fail "Invalid Smash index"

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Smash where
  bimap f g = \case
    Nada -> Nada
    Smash a b -> Smash (f a) (g b)

instance Biapplicative Smash where
  bipure = Smash

  Smash f g <<*>> Smash a b = Smash (f a) (g b)
  _ <<*>> _ = Nada

instance Bifoldable Smash where
  bifoldMap f g = \case
    Nada -> mempty
    Smash a b -> f a `mappend` g b

instance Bitraversable Smash where
  bitraverse f g = \case
    Nada -> pure Nada
    Smash a b -> Smash <$> f a <*> g b
