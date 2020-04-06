{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module       : Data.Can
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the definition for the 'Can' datatype. In
-- practice, this type is isomorphic to 'Maybe' 'These' - the type with
-- two possibly non-exclusive values and an empty case.
module Data.Can
( -- * Datatypes
  -- $general
  Can(..)
  -- * Combinators
, canFst
, canSnd
, isOne
, isEno
, isTwo
, isNon
  -- ** Eliminators
, can
  -- * Folding
, foldOnes
, foldEnos
, foldTwos
, gatherCans
  -- * Filtering
, ones
, enos
, twos
, filterOnes
, filterEnos
, filterTwos
, filterNons
  -- * Curry & Uncurry
, canCurry
, canUncurry
  -- * Partitioning
, partitionCans
, partitionAll
, partitionEithers
, mapCans
  -- * Distributivity
, distributeCan
, codistributeCan
  -- * Associativity
, reassocLR
, reassocRL
  -- * Symmetry
, swapCan
) where


import Control.Applicative (Alternative(..))

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import qualified Data.Either as E
import Data.Foldable
import Data.Hashable

import GHC.Generics

{- $general

Categorically, the 'Can' datatype represents the
<https://ncatlab.org/nlab/show/pointed+object#limits_and_colimits pointed product>
in the category Hask* of pointed Hask types. The category Hask* consists of
Hask types affixed with a dedicated base point of an object along with the object.
In Hask*, this is equivalent to `1 + a`, or 'Maybe a' in Hask. Hence, the product is
`(1 + a) * (1 + b) ~ 1 + a + b + a*b`, or `Maybe (Either (Either a b) (a,b))` in Hask. Pictorially, you can visualize
this as:


@
'Can':
        a
        |
Non +---+---+ (a,b)
        |
        b
@


The fact that we can think about 'Can' as your average product gives us
some reasoning power about how this thing will be able to interact with the
coproduct in Hask*, called 'Wedge'. Namely, facts about currying
'Can a b -> c ~ a -> b -> c' and distributivity over 'Wedge'
along with other facts about its associativity, commutativity, and
any other analogy with `(,)` that you can think of.
-}


-- | The 'Can' data type represents values with two non-exclusive
-- possibilities, as well as an empty case. This is a product of pointed types -
-- i.e. of 'Maybe' values. The result is a type, 'Can a b', which is isomorphic
-- to 'Maybe (These a b)'.
--
data Can a b = Non | One a | Eno b | Two a b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )

-- -------------------------------------------------------------------- --
-- Eliminators

-- | Case elimination for the 'Can' datatype
--
can
    :: c
      -- ^ default value to supply for the 'Non' case
    -> (a -> c)
      -- ^ eliminator for the 'One' case
    -> (b -> c)
      -- ^ eliminator for the 'Eno' case
    -> (a -> b -> c)
      -- ^ eliminator for the 'Two' case
    -> Can a b
    -> c
can c _ _ _ Non = c
can _ f _ _ (One a) = f a
can _ _ g _ (Eno b) = g b
can _ _ _ h (Two a b) = h a b

-- -------------------------------------------------------------------- --
-- Combinators

-- | Project the left value of a 'Can' datatype. This is analogous
-- to 'fst' for '(,)'.
--
canFst :: Can a b -> Maybe a
canFst = \case
  One a -> Just a
  Two a _ -> Just a
  _ -> Nothing

-- | Project the right value of a 'Can' datatype. This is analogous
-- to 'snd' for '(,)'.
--
canSnd :: Can a b -> Maybe b
canSnd = \case
  Eno b -> Just b
  Two _ b -> Just b
  _ -> Nothing

-- | Detect if a 'Can' is a 'One' case.
--
isOne :: Can a b -> Bool
isOne (One _) = True
isOne _ = False

-- | Detect if a 'Can' is a 'Eno' case.
--
isEno :: Can a b -> Bool
isEno (Eno _) = True
isEno _ = False

-- | Detect if a 'Can' is a 'Two' case.
--
isTwo :: Can a b -> Bool
isTwo (Two _ _) = True
isTwo _ = False

-- | Detect if a 'Can' is a 'Non' case.
--
isNon :: Can a b -> Bool
isNon Non = True
isNon _ = False

-- -------------------------------------------------------------------- --
-- Filtering

-- | Given a 'Foldable' of 'Can's, collect the values of the
-- 'One' cases, if any.
--
ones :: Foldable f => f (Can a b) -> [a]
ones = foldr go []
  where
    go (One a) acc = a:acc
    go _ acc = acc

-- | Given a 'Foldable' of 'Can's, collect the values of the
-- 'Eno' cases, if any.
--
enos :: Foldable f => f (Can a b) -> [b]
enos = foldr go []
  where
    go (Eno a) acc = a:acc
    go _ acc = acc

-- | Given a 'Foldable' of 'Can's, collect the values of the
-- 'Two' cases, if any.
--
twos :: Foldable f => f (Can a b) -> [(a,b)]
twos = foldr go []
  where
    go (Two a b) acc = (a,b):acc
    go _ acc = acc

-- | Filter the 'One' cases of a 'Foldable' of 'Can' values.
--
filterOnes :: Foldable f => f (Can a b) -> [Can a b]
filterOnes = foldr go []
  where
    go (One _) acc = acc
    go t acc = t:acc

-- | Filter the 'Eno' cases of a 'Foldable' of 'Can' values.
--
filterEnos :: Foldable f => f (Can a b) -> [Can a b]
filterEnos = foldr go []
  where
    go (Eno _) acc = acc
    go t acc = t:acc

-- | Filter the 'Two' cases of a 'Foldable' of 'Can' values.
--
filterTwos :: Foldable f => f (Can a b) -> [Can a b]
filterTwos = foldr go []
  where
    go (Two _ _) acc = acc
    go t acc = t:acc

-- | Filter the 'Non' cases of a 'Foldable' of 'Can' values.
--
filterNons :: Foldable f => f (Can a b) -> [Can a b]
filterNons = foldr go []
  where
    go Non acc = acc
    go t acc = t:acc

-- -------------------------------------------------------------------- --
-- Folding

-- | Fold over the 'One' cases of a 'Foldable' of 'Can's by some
-- accumulating function.
--
foldOnes :: Foldable f => (a -> m -> m) -> m -> f (Can a b) -> m
foldOnes k = foldr go
  where
    go (One a) acc = k a acc
    go _ acc = acc

-- | Fold over the 'Eno' cases of a 'Foldable' of 'Can's by some
-- accumulating function.
--
foldEnos :: Foldable f => (b -> m -> m) -> m -> f (Can a b) -> m
foldEnos k = foldr go
  where
    go (Eno b) acc = k b acc
    go _ acc = acc

-- | Fold over the 'Two' cases of a 'Foldable' of 'Can's by some
-- accumulating function.
--
foldTwos :: Foldable f => (a -> b -> m -> m) -> m -> f (Can a b) -> m
foldTwos k = foldr go
  where
    go (Two a b) acc = k a b acc
    go _ acc = acc

-- | Gather a 'Can' of two lists and produce a list of 'Can' values,
-- mapping the 'Non' case to the empty list, One' case to a list
-- of 'One's, the 'Eno' case to a list of 'Eno's, or zipping 'Two'
-- along both lists.
--
gatherCans :: Can [a] [b] -> [Can a b]
gatherCans Non = []
gatherCans (One as) = fmap One as
gatherCans (Eno bs) = fmap Eno bs
gatherCans (Two as bs) = zipWith Two as bs

-- -------------------------------------------------------------------- --
-- Partitioning

-- | Partition a list of 'Can' values into a triple of lists of
-- all of their constituent parts
--
partitionAll :: Foldable f => f (Can a b) -> ([a], [b], [(a,b)])
partitionAll = flip foldr mempty $ \aa ~(as, bs, cs) -> case aa of
    Non -> (as, bs, cs)
    One a -> (a:as, bs, cs)
    Eno b -> (as, b:bs, cs)
    Two a b -> (as, bs, (a,b):cs)

-- | Partition a list of 'Either' values, separating them into
-- a 'Can' value of lists of left and right values, or 'Non' in the
-- case of an empty list.
--
partitionEithers :: Foldable f => f (Either a b) -> Can [a] [b]
partitionEithers = go . E.partitionEithers . toList
  where
    go ([], []) = Non
    go (ls, []) = One ls
    go ([], rs) = Eno rs
    go (ls, rs) = Two ls rs

-- | Given a 'Foldable' of 'Can's, partition it into a tuple of alternatives
-- their parts.
--
partitionCans
    :: forall f t a b
    . ( Foldable t
      , Alternative f
      )
    => t (Can a b) -> (f a, f b)
partitionCans = foldr go (empty, empty)
  where
    go Non acc = acc
    go (One a) (as, bs) = (pure a <|> as, bs)
    go (Eno b) (as, bs) = (as, pure b <|> bs)
    go (Two a b) (as, bs) = (pure a <|> as, pure b <|> bs)

-- | Partition a structure by mapping its contents into 'Can's,
-- and folding over '(<|>)'.
--
mapCans
    :: forall f t a b c
    . ( Alternative f
      , Traversable t
      )
    => (a -> Can b c)
    -> t a
    -> (f b, f c)
mapCans f = partitionCans . fmap f

-- -------------------------------------------------------------------- --
-- Distributivity

-- | Distribute a 'Can' value over a product.
--
distributeCan :: Can (a,b) c -> (Can a c, Can b c)
distributeCan = \case
    Non -> (Non, Non)
    One (a,b) -> (One a, One b)
    Eno c -> (Eno c, Eno c)
    Two (a,b) c -> (Two a c, Two b c)

-- | Codistribute a coproduct over a 'Can' value.
--
codistributeCan :: Either (Can a c) (Can b c) -> Can (Either a b) c
codistributeCan = \case
    Left ac -> case ac of
      Non -> Non
      One a -> One (Left a)
      Eno c -> Eno c
      Two a c -> Two (Left a) c
    Right bc -> case bc of
      Non -> Non
      One b -> One (Right b)
      Eno c -> Eno c
      Two b c -> Two (Right b) c

-- -------------------------------------------------------------------- --
-- Associativity

-- | Re-associate a 'Can' of cans from left to right.
--
reassocLR :: Can (Can a b) c -> Can a (Can b c)
reassocLR = \case
    Non -> Non
    One c -> case c of
      Non -> Eno Non
      One a -> One a
      Eno b -> Eno (One b)
      Two a b -> Two a (One b)
    Eno c -> Eno (Eno c)
    Two c d -> case c of
      Non -> Eno (Eno d)
      One a -> Two a (Eno d)
      Eno b -> Eno (Two b d)
      Two a b -> Two a (Two b d)

-- | Re-associate a 'Can' of cans from right to left.
--
reassocRL :: Can a (Can b c) -> Can (Can a b) c
reassocRL = \case
    Non -> Non
    One a -> One (One a)
    Eno c -> case c of
      Non -> One Non
      One b -> One (Eno b)
      Eno d -> Eno d
      Two b d -> Two (Eno b) d
    Two a c -> case c of
      Non -> One (One a)
      One b -> One (Two a b)
      Eno d -> Two (One a) d
      Two b d -> Two (Two a b) d

-- -------------------------------------------------------------------- --
-- Symmetry

-- | Swap the positions of values in a 'Can'.
--
swapCan :: Can a b -> Can b a
swapCan = \case
    Non -> Non
    One a -> Eno a
    Eno b -> One b
    Two a b -> Two b a

-- -------------------------------------------------------------------- --
-- Curry & Uncurry

-- | Curry a function from a 'Can' to a 'Maybe' value, resulting in a
-- function of curried 'Maybe' values. This is analogous to currying
-- for '(->)'.
--
canCurry :: (Can a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
canCurry k ma mb = case (ma, mb) of
    (Nothing, Nothing) -> k Non
    (Just a, Nothing) -> k (One a)
    (Nothing, Just b) -> k (Eno b)
    (Just a, Just b) -> k (Two a b)

-- | "Uncurry" a function from a 'Can' to a 'Maybe' value, resulting in a
-- function of curried 'Maybe' values. This is analogous to uncurrying
-- for '(->)'.
--
canUncurry :: (Maybe a -> Maybe b -> Maybe c) -> Can a b -> Maybe c
canUncurry k = \case
    Non -> k Nothing Nothing
    One a -> k (Just a) Nothing
    Eno b -> k Nothing (Just b)
    Two a b -> k (Just a) (Just b)

-- -------------------------------------------------------------------- --
-- Std instances


instance (Hashable a, Hashable b) => Hashable (Can a b)

instance Functor (Can a) where
  fmap _ Non = Non
  fmap _ (One a) = One a
  fmap f (Eno b) = Eno (f b)
  fmap f (Two a b) = Two a (f b)

instance Foldable (Can a) where
  foldMap k (Eno b) = k b
  foldMap k (Two _ b) = k b
  foldMap _ _ = mempty

instance Traversable (Can a) where
  traverse k = \case
    Non -> pure Non
    One a -> pure (One a)
    Eno b -> Eno <$> k b
    Two a b -> Two a <$> k b

instance Semigroup a => Applicative (Can a) where
  pure = Eno

  _ <*> Non = Non
  Non <*> _ = Non
  One a <*> _ = One a
  Eno _ <*> One b = One b
  Eno f <*> Eno a = Eno (f a)
  Eno f <*> Two a b = Two a (f b)
  Two a _ <*> One b = One (a <> b)
  Two a f <*> Eno b = Two a (f b)
  Two a f <*> Two b c = Two (a <> b) (f c)

instance Semigroup a => Monad (Can a) where
  return = pure
  (>>) = (*>)

  Non >>= _ = Non
  One a >>= _ = One a
  Eno b >>= k = k b
  Two a b >>= k = case k b of
    Non -> Non
    One c -> One (a <> c)
    Eno c -> Eno c
    Two c d -> Two (a <> c) d

instance (Semigroup a, Semigroup b) => Semigroup (Can a b) where
  Non <> b = b
  b <> Non = b
  One a <> One b = One (a <> b)
  One a <> Eno b = Two a b
  One a <> Two b c = Two (a <> b) c
  Eno a <> Eno b = Eno (a <> b)
  Eno b <> One a = Two a b
  Eno b <> Two a c = Two a (b <> c)
  Two a b <> Two c d = Two (a <> c) (b <> d)
  Two a b <> One c = Two (a <> c) b
  Two a b <> Eno c = Two a (b <> c)


instance (Semigroup a, Semigroup b) => Monoid (Can a b) where
  mempty = Non

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Can where
  bimap f g = \case
    Non -> Non
    One a -> One (f a)
    Eno b -> Eno (g b)
    Two a b -> Two (f a) (g b)

instance Bifoldable Can where
  bifoldMap f g = \case
    Non -> mempty
    One a -> f a
    Eno b -> g b
    Two a b -> f a <> g b

instance Bitraversable Can where
  bitraverse f g = \case
    Non -> pure Non
    One a -> One <$> f a
    Eno b -> Eno <$> g b
    Two a b -> Two <$> f a <*> g b
