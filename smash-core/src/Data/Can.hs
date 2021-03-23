{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Can
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : CPP, RankNTypes, TypeApplications
--
-- This module contains the definition for the 'Can' datatype. In
-- practice, this type is isomorphic to 'Maybe' 'These' - the type with
-- two possibly non-exclusive values and an empty case.
--
module Data.Can
( -- * Datatypes
  -- $general
  Can(..)
  -- ** Type synonyms
, type (⊗)
  -- * Combinators
, canFst
, canSnd
, isOne
, isEno
, isTwo
, isNon
  -- ** Eliminators
, can
, canWithMerge
, canEach
, canEachA
  -- * Folding and Unfolding
, foldOnes
, foldEnos
, foldTwos
, gatherCans
, unfoldr
, unfoldrM
, iterateUntil
, iterateUntilM
, accumUntil
, accumUntilM
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


import Control.Applicative (Alternative(..), liftA2)
import Control.DeepSeq (NFData(..))
import Control.Monad.Zip
import Control.Monad

import Data.Biapplicative
import Data.Bifoldable
import Data.Binary (Binary(..))
import Data.Bitraversable
import Data.Data
import qualified Data.Either as E
import Data.Functor.Classes
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable

import GHC.Generics
import GHC.Read

import qualified Language.Haskell.TH.Syntax as TH

import Data.Smash.Internal

import Text.Read hiding (get)



{- $general

Categorically, the 'Can' datatype represents the
<https://ncatlab.org/nlab/show/pointed+object#limits_and_colimits pointed product>
in the category Hask* of pointed Hask types. The category Hask* consists of
Hask types affixed with a dedicated base point of an object along with the object - i.e. @'Maybe' a@ in Hask. Hence, the product is
@(1 + a) * (1 + b) ~ 1 + a + b + a*b@, or @'Maybe' ('These' a b)@ in Hask. Pictorially, you can visualize
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
@Can a b -> c ~ a -> b -> c@ and distributivity over 'Wedge'
along with other facts about its associativity, commutativity, and
any other analogy with @(',')@ that you can think of.
-}


-- | The 'Can' data type represents values with two non-exclusive
-- possibilities, as well as an empty case. This is a product of pointed types -
-- i.e. of 'Maybe' values. The result is a type, @'Can' a b@, which is isomorphic
-- to @'Maybe' ('These' a b)@.
--
data Can a b = Non | One a | Eno b | Two a b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    , TH.Lift
    )

-- | A type operator synonym for 'Can'
--
type a ⊗ b = Can a b

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

-- | Case elimination for the 'Can' datatype, with uniform behaviour.
--
canWithMerge
    :: c
      -- ^ default value to supply for the 'Non' case
    -> (a -> c)
      -- ^ eliminator for the 'One' case
    -> (b -> c)
      -- ^ eliminator for the 'Eno' case
    -> (c -> c -> c)
      -- ^ merger for the 'Two' case
    -> Can a b
    -> c
canWithMerge c _ _ _ Non = c
canWithMerge _ f _ _ (One a) = f a
canWithMerge _ _ g _ (Eno b) = g b
canWithMerge _ f g m (Two a b) = m (f a) (g b)

-- | Case elimination for the 'Can' datatype, with uniform behaviour over a
-- 'Monoid' result.
--
canEach
    :: Monoid c
    => (a -> c)
      -- ^ eliminator for the 'One' case
    -> (b -> c)
      -- ^ eliminator for the 'Eno' case
    -> Can a b
    -> c
canEach f g = canWithMerge mempty f g (<>)

-- | Case elimination for the 'Can' datatype, with uniform behaviour over a
-- 'Monoid' result in the context of an 'Applicative'.
--
canEachA
    :: Applicative m
    => Monoid c
    => (a -> m c)
      -- ^ eliminator for the 'One' case
    -> (b -> m c)
      -- ^ eliminator for the 'Eno' case
    -> Can a b
    -> m c
canEachA f g = canWithMerge (pure mempty) f g (liftA2 (<>))

-- -------------------------------------------------------------------- --
-- Combinators

-- | Project the left value of a 'Can' datatype. This is analogous
-- to 'fst' for @(',')@.
--
canFst :: Can a b -> Maybe a
canFst = \case
  One a -> Just a
  Two a _ -> Just a
  _ -> Nothing

-- | Project the right value of a 'Can' datatype. This is analogous
-- to 'snd' for @(',')@.
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

-- | Unfold from right to left into a pointed product. For a variant
-- that accumulates in the seed instead of just updating with a
-- new value, see 'accumUntil' and 'accumUntilM'.
--
unfoldr :: Alternative f => (b -> Can a b) -> b -> f a
unfoldr f = runIdentity . unfoldrM (pure . f)

-- | Unfold from right to left into a monadic computation over a pointed product
--
unfoldrM :: (Monad m, Alternative f) => (b -> m (Can a b)) -> b -> m (f a)
unfoldrM f b = f b >>= \case
    Non -> pure empty
    One a -> (pure a <|>) <$> unfoldrM f b
    Eno b' -> unfoldrM f b'
    Two a b' -> (pure a <|>) <$> unfoldrM f b'

-- | Iterate on a seed, accumulating a result. See 'iterateUntilM' for
-- more details.
--
iterateUntil :: Alternative f => (b -> Can a b) -> b -> f a
iterateUntil f = runIdentity . iterateUntilM (pure . f)

-- | Iterate on a seed, which may result in one of four scenarios:
--
--   1. The function yields a @Non@ value, which terminates the
--      iteration.
--
--   2. The function yields a @One@ value.
--
--   3. The function yields a @Eno@ value, which changes the seed
--      and iteration continues with the new seed.
--
--   4. The function yields the @a@ value of a @Two@ case.
--
iterateUntilM
    :: Monad m
    => Alternative f
    => (b -> m (Can a b))
    -> b
    -> m (f a)
iterateUntilM f b = f b >>= \case
    Non -> pure empty
    One a -> pure (pure a)
    Eno b' -> iterateUntilM f b'
    Two a _ -> pure (pure a)

-- | Iterate on a seed, accumulating values and monoidally
-- updating the seed with each update.
--
accumUntil
    :: Alternative f
    => Monoid b
    => (b -> Can a b)
    -> f a
accumUntil f = runIdentity (accumUntilM (pure . f))

-- | Iterate on a seed, accumulating values and monoidally
-- updating a seed within a monad.
--
accumUntilM
    :: Monad m
    => Alternative f
    => Monoid b
    => (b -> m (Can a b))
    -> m (f a)
accumUntilM f = go mempty
  where
    go b = f b >>= \case
      Non -> pure empty
      One a -> (pure a <|>) <$> go b
      Eno b' -> go (b' `mappend` b)
      Two a b' -> (pure a <|>) <$> go (b' `mappend` b)

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
    :: Foldable t
    => Alternative f
    => t (Can a b)
    -> (f a, f b)
partitionCans = foldr go (empty, empty)
  where
    go Non acc = acc
    go (One a) (as, bs) = (pure a <|> as, bs)
    go (Eno b) (as, bs) = (as, pure b <|> bs)
    go (Two a b) (as, bs) = (pure a <|> as, pure b <|> bs)

-- | Partition a structure by mapping its contents into 'Can's,
-- and folding over @('<|>')@.
--
mapCans
    :: Traversable t
    => Alternative f
    => (a -> Can b c)
    -> t a
    -> (f b, f c)
mapCans f = partitionCans . fmap f

-- -------------------------------------------------------------------- --
-- Distributivity

-- | Distribute a 'Can' value over a product.
--
distributeCan :: Can (a,b) c -> (Can a c, Can b c)
distributeCan = unzipFirst

-- | Codistribute a coproduct over a 'Can' value.
--
codistributeCan :: Either (Can a c) (Can b c) -> Can (Either a b) c
codistributeCan = undecideFirst

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
swapCan = can Non Eno One (flip Two)

-- -------------------------------------------------------------------- --
-- Curry & Uncurry

-- | Curry a function from a 'Can' to a 'Maybe' value, resulting in a
-- function of curried 'Maybe' values. This is analogous to currying
-- for @('->')@.
--
canCurry :: (Can a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
canCurry k ma mb = case (ma, mb) of
    (Nothing, Nothing) -> k Non
    (Just a, Nothing) -> k (One a)
    (Nothing, Just b) -> k (Eno b)
    (Just a, Just b) -> k (Two a b)

-- | "Uncurry" a function from a 'Can' to a 'Maybe' value, resulting in a
-- function of curried 'Maybe' values. This is analogous to uncurrying
-- for @('->')@.
--
canUncurry :: (Maybe a -> Maybe b -> Maybe c) -> Can a b -> Maybe c
canUncurry k = \case
    Non -> k Nothing Nothing
    One a -> k (Just a) Nothing
    Eno b -> k Nothing (Just b)
    Two a b -> k (Just a) (Just b)

-- -------------------------------------------------------------------- --
-- Std instances

instance Eq2 Can where
  liftEq2 _ _ Non Non = True
  liftEq2 f _ (One a) (One c) = f a c
  liftEq2 _ g (Eno b) (Eno d) = g b d
  liftEq2 f g (Two a b) (Two c d) = f a c && g b d
  liftEq2 _ _ _ _ = False

instance Ord2 Can where
  liftCompare2 _ _ Non Non = EQ
  liftCompare2 _ _ Non _ = LT
  liftCompare2 _ _ _ Non = GT
  liftCompare2 f _ (One a) (One c) = f a c
  liftCompare2 _ g (Eno b) (Eno d) = g b d
  liftCompare2 f g (Two a b) (Two c d) = f a c <> g b d
  liftCompare2 _ _ One{} _ = LT
  liftCompare2 _ _ Eno{} Two{} = LT
  liftCompare2 _ _ Eno{} One{} = GT
  liftCompare2 _ _ Two{} _ = GT

instance Show2 Can where
  liftShowsPrec2 _ _ _ _ _ Non = showString "Non"
  liftShowsPrec2 f _ _ _ d (One a) = showsUnaryWith f "One" d a
  liftShowsPrec2 _ _ g _ d (Eno b) = showsUnaryWith g "Eno" d b
  liftShowsPrec2 f _ g _ d (Two a b) = showsBinaryWith f g "Two" d a b

instance Read2 Can where
  liftReadPrec2 rpa _ rpb _ = nonP <|> oneP <|> enoP <|> twoP
    where
      nonP = Non <$ expectP (Ident "Non")
      oneP = readData $ readUnaryWith rpa "One" One
      enoP = readData $ readUnaryWith rpb "Eno" Eno
      twoP = readData $ readBinaryWith rpa rpb "Two" Two

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
  mappend = (<>)

instance (NFData a, NFData b) => NFData (Can a b) where
    rnf Non = ()
    rnf (One a) = rnf a
    rnf (Eno b) = rnf b
    rnf (Two a b) = rnf a `seq` rnf b

instance (Binary a, Binary b) => Binary (Can a b) where
  put Non = put @Int 0
  put (One a) = put @Int 1 >> put a
  put (Eno b) = put @Int 2 >> put b
  put (Two a b) = put @Int 3 >> put a >> put b

  get = get @Int >>= \case
    0 -> pure Non
    1 -> One <$> get
    2 -> Eno <$> get
    3 -> Two <$> get <*> get
    _ -> fail "Invalid Can index"

instance Semigroup a => MonadZip (Can a) where
  mzipWith f a b = f <$> a <*> b

instance Semigroup a => Alternative (Can a) where
  empty = Non
  Non <|> c = c
  c <|> Non = c
  One a <|> One b = One (a <> b)
  One a <|> Eno b = Two a b
  One a <|> Two b c = Two (a <> b) c
  Eno a <|> One b = Two b a
  Eno _ <|> c = c
  Two a b <|> One c = Two (a <> c) b
  Two a _ <|> Eno b = Two a b
  Two a _ <|> Two b c = Two (a <> b) c

instance Semigroup a => MonadPlus (Can a)

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Can where
  bimap f g = \case
    Non -> Non
    One a -> One (f a)
    Eno b -> Eno (g b)
    Two a b -> Two (f a) (g b)

instance Biapplicative Can where
  bipure = Two

  One f <<*>> One a = One (f a)
  One f <<*>> Two a _ = One (f a)
  Eno g <<*>> Eno b = Eno (g b)
  Eno g <<*>> Two _ b = Eno (g b)
  Two f _ <<*>> One a = One (f a)
  Two _ g <<*>> Eno b = Eno (g b)
  Two f g <<*>> Two a b = Two (f a) (g b)
  _ <<*>> _ = Non

instance Bifoldable Can where
  bifoldMap f g = \case
    Non -> mempty
    One a -> f a
    Eno b -> g b
    Two a b -> f a `mappend` g b

instance Bitraversable Can where
  bitraverse f g = \case
    Non -> pure Non
    One a -> One <$> f a
    Eno b -> Eno <$> g b
    Two a b -> Two <$> f a <*> g b
