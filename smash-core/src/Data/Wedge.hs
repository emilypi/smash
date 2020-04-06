{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module       : Data.Wedge
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the definition for the 'Wedge' datatype. In
-- practice, this type is isomorphic to 'Maybe (Either a b)' - the type with
-- two possibly non-exclusive values and an empty case.
module Data.Wedge
( -- * Datatypes
  -- $general
  Wedge(..)
  -- * Combinators
, quotWedge
, wedgeLeft
, wedgeRight
, fromWedge
, toWedge
, isHere
, isThere
, isNowhere
  -- ** Eliminators
, wedge
  -- ** Filtering
, heres
, theres
, filterHeres
, filterTheres
, filterNowheres
  -- ** Folding
, foldHeres
, foldTheres
, gatherWedges
  -- ** Partitioning
, partitionWedges
, mapWedges
  -- ** Distributivity
, distributeWedge
, codistributeWedge
  -- ** Associativity
, reassocLR
, reassocRL
  -- ** Symmetry
, swapWedge
) where


import Control.Applicative (Alternative(..))

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import Data.Hashable
import Data.Semigroup (Semigroup(..))

import GHC.Generics

{- $general

Categorically, the 'Wedge' datatype represents the coproduct (like, 'Either')
in the category Hask* of pointed Hask types, called a <https://ncatlab.org/nlab/show/wedge+sum wedge sum>.
The category Hask* consists of Hask types affixed with
a dedicated base point along with an object. In Hask, this is
equivalent to `1 + a`, also known as 'Maybe a'. Because we can conflate
basepoints of different types (there is only one @Nothing@ type), the wedge sum is
can be viewed as the type `1 + a + b`, or `Maybe (Either a b)` in Hask.
Pictorially, one can visualize this as:


@
'Wedge':
                a
                |
Nowhere +-------+
                |
                b
@


The fact that we can think about 'Wedge' as a coproduct gives us
some reasoning power about how a 'Wedge' will interact with the
product in Hask*, called 'Can'. Namely, we know that a product of a type and a
coproduct, `a * (b + c)`, is equivalent to `(a + b) * (a + c)`. Additioally,
we may derive other facts about its associativity, distributivity, commutativity, and
any more. As an exercise, think of soemthing `Either` can do. Now do it with 'Wedge'!

-}

-- | The 'Wedge' data type represents values with two exclusive
-- possibilities, and an empty case. This is a coproduct of pointed
-- types - i.e. of 'Maybe' values. The result is a type, 'Wedge a b',
-- which is isomorphic to 'Maybe (Either a b)'.
--
data Wedge a b = Nowhere | Here a | There b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )

-- -------------------------------------------------------------------- --
-- Eliminators

-- | Case elimination for the 'Wedge' datatype.
--
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
-- Combinators

-- | Given two possible pointed types, produce a 'Wedge' by
-- considering the left case, the right case, and mapping their
-- 'Nothing' cases to 'Nowhere'. This is a pushout of pointed
-- types `A <- * -> B`.
--
quotWedge :: Either (Maybe a) (Maybe b) -> Wedge a b
quotWedge (Left a) = maybe Nowhere Here a
quotWedge (Right b) = maybe Nowhere There b

-- | Convert a 'Wedge a b' into a 'Maybe (Either a b)' value.
--
fromWedge :: Wedge a b -> Maybe (Either a b)
fromWedge Nowhere = Nothing
fromWedge (Here a) = Just (Left a)
fromWedge (There b) = Just (Right b)

-- | Convert a 'Maybe (Either a b)' value into a 'Wedge'
--
toWedge :: Maybe (Either a b) -> Wedge a b
toWedge Nothing = Nowhere
toWedge (Just e) = either Here There e

-- | Inject a 'Maybe' value into the 'Here' case of a 'Wedge',
-- or 'Nowhere' if the empty case is given. This is analogous to the
-- 'Left' constructor for 'Either'.
--
wedgeLeft :: Maybe a -> Wedge a b
wedgeLeft Nothing = Nowhere
wedgeLeft (Just a) = Here a

-- | Inject a 'Maybe' value into the 'There' case of a 'Wedge',
-- or 'Nowhere' if the empty case is given. This is analogous to the
-- 'Right' constructor for 'Either'.
--
wedgeRight :: Maybe b -> Wedge a b
wedgeRight Nothing = Nowhere
wedgeRight (Just b) = There b

-- | Detect if a 'Wedge' is a 'Here' case.
--
isHere :: Wedge a b -> Bool
isHere = \case
  Here _ -> True
  _ -> False

-- | Detect if a 'Wedge' is a 'There' case.
--
isThere :: Wedge a b -> Bool
isThere = \case
  There _ -> True
  _ -> False

-- | Detect if a 'Wedge' is a 'Nowhere' empty case.
--
isNowhere :: Wedge a b -> Bool
isNowhere = \case
  Nowhere -> True
  _ -> False

-- -------------------------------------------------------------------- --
-- Filtering


-- | Given a 'Foldable' of 'Wedge's, collect the 'Here' cases, if any.
--
heres :: Foldable f => f (Wedge a b) -> [a]
heres = foldr go mempty
  where
    go (Here a) acc = a:acc
    go _ acc = acc

-- | Given a 'Foldable' of 'Wedge's, collect the 'There' cases, if any.
--
theres :: Foldable f => f (Wedge a b) -> [b]
theres = foldr go mempty
  where
    go (There b) acc = b:acc
    go _ acc = acc

-- | Filter the 'Here' cases of a 'Foldable' of 'Wedge's.
--
filterHeres :: Foldable f => f (Wedge a b) -> [Wedge a b]
filterHeres = foldr go mempty
  where
    go (Here _) acc = acc
    go ab acc = ab:acc

-- | Filter the 'There' cases of a 'Foldable' of 'Wedge's.
--
filterTheres :: Foldable f => f (Wedge a b) -> [Wedge a b]
filterTheres = foldr go mempty
  where
    go (There _) acc = acc
    go ab acc = ab:acc

-- | Filter the 'Nowhere' cases of a 'Foldable' of 'Wedge's.
--
filterNowheres :: Foldable f => f (Wedge a b) -> [Wedge a b]
filterNowheres = foldr go mempty
  where
    go Nowhere acc = acc
    go ab acc = ab:acc

-- -------------------------------------------------------------------- --
-- Filtering

-- | Fold over the 'Here' cases of a 'Foldable' of 'Wedge's by some
-- accumulating function.
--
foldHeres :: Foldable f => (a -> m -> m) -> m -> f (Wedge a b) -> m
foldHeres k = foldr go
  where
    go (Here a) acc = k a acc
    go _ acc = acc

-- | Fold over the 'There' cases of a 'Foldable' of 'Wedge's by some
-- accumulating function.
--
foldTheres :: Foldable f => (b -> m -> m) -> m -> f (Wedge a b) -> m
foldTheres k = foldr go
  where
    go (There b) acc = k b acc
    go _ acc = acc


-- | Given a 'Wedge' of lists, produce a list of wedges by mapping
-- the list of 'as' to 'Here' values, or the list of 'bs' to 'There'
-- values.
--
gatherWedges :: Wedge [a] [b] -> [Wedge a b]
gatherWedges Nowhere = []
gatherWedges (Here as) = fmap Here as
gatherWedges (There bs) = fmap There bs

-- -------------------------------------------------------------------- --
-- Partitioning

-- | Given a 'Foldable' of 'Wedge's, partition it into a tuple of alternatives
-- their parts.
--
partitionWedges
    :: forall f t a b
    . ( Foldable t
      , Alternative f
      )
    => t (Wedge a b) -> (f a, f b)
partitionWedges = foldr go (empty, empty)
  where
    go Nowhere acc = acc
    go (Here a) (as, bs) = (pure a <|> as, bs)
    go (There b) (as, bs) = (as, pure b <|> bs)

-- | Partition a structure by mapping its contents into 'Wedge's,
-- and folding over '(<|>)'.
--
mapWedges
    :: forall f t a b c
    . ( Alternative f
      , Traversable t
      )
    => (a -> Wedge b c)
    -> t a
    -> (f b, f c)
mapWedges f = partitionWedges . fmap f

-- -------------------------------------------------------------------- --
-- Associativity

-- | Re-associate a 'Wedge' of 'Wedge's from left to right.
--
reassocLR :: Wedge (Wedge a b) c -> Wedge a (Wedge b c)
reassocLR = \case
    Nowhere -> Nowhere
    Here w -> case w of
      Nowhere -> There Nowhere
      Here a -> Here a
      There b -> There (Here b)
    There c -> There (There c)

-- | Re-associate a 'Wedge' of 'Wedge's from left to right.
--
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

-- | Distribute a 'Wedge' over a product.
--
distributeWedge :: Wedge (a,b) c -> (Wedge a c, Wedge b c)
distributeWedge = \case
  Nowhere -> (Nowhere, Nowhere)
  Here (a,b) -> (Here a, Here b)
  There c -> (There c, There c)

-- | Codistribute 'Wedge's over a coproduct
--
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

-- | Swap the positions of the @a@'s and the @b@'s in a 'Wedge'.
--
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
