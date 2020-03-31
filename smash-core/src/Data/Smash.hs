{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Data.Smash
( -- * Datatypes
  Smash(..)
  -- * Combinators
  -- ** Curry & Uncurry
  -- ** Eliminators
, smash
, fromSmash
, joinSmash
, joinWith
  -- ** Partitioning
, partition
, partitionAll
, partitionEithers
  -- ** Distributivity
, distributeSmash
, codistributeSmash
  -- ** Associativity
, reassocLR
, reassocRL
  -- ** Symmetry
, swapSmash
) where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import qualified Data.Either as E
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Typeable

import GHC.Generics


-- | The 'Smash' data type represents values with two non-exclusive
-- possibilities, as well as an empty case
--
--
data Smash a b = Non | One a | Eno b | Two a b
  deriving
    ( Eq, Ord, Read, Show
    , Generic, Generic1
    , Typeable, Data
    )


-- -------------------------------------------------------------------- --
-- Curry & Uncurry

smashCurry :: (Smash a b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
smashCurry k ma mb = case (ma, mb) of
    (Nothing, Nothing) -> k Non
    (Just a, Nothing) -> k (One a)
    (Nothing, Just b) -> k (Eno b)
    (Just a, Just b) -> k (Two a b)

smashUncurry :: (Maybe a -> Maybe b -> Maybe c) -> Smash a b -> Maybe c
smashUncurry k = \case
    Non -> k Nothing Nothing
    One a -> k (Just a) Nothing
    Eno b -> k Nothing (Just b)
    Two a b -> k (Just a) (Just b)

-- -------------------------------------------------------------------- --
-- Eliminators

-- | Case eliminator for the 'Smash' datatype
--
smash
    :: c
      -- ^ default value to supply for the 'Non' case
    -> (a -> c)
      -- ^ eliminator for the 'One' case
    -> (b -> c)
      -- ^ eliminator for the 'Eno' case
    -> (a -> b -> c)
      -- ^ eliminator for the 'Two' case
    -> Smash a b
    -> c
smash c _ _ _ Non = c
smash _ f _ _ (One a) = f a
smash _ _ g _ (Eno b) = g b
smash _ _ _ h (Two a b) = h a b

-- | Given two default values, create a 'Maybe' value containing
-- either nothing, or just a tuple.
--
fromSmash :: a -> b -> Smash a b -> Maybe (a,b)
fromSmash a b = smash Nothing (Just . (,b)) (Just . (a,)) (\c d -> Just (c,d))

-- | Merge the values of a 'Smash', using some default value as a local unit.
--
joinSmash :: a -> (a -> a -> a) -> Smash a a -> a
joinSmash a k = smash a (k a) (k a) k

-- | Merge the values of a 'Smash', using some default value as a local unit,
-- providing a conversion to 'bimap' with before merging.
--
joinWith
    :: c
    -> (a -> c)
    -> (b -> c)
    -> (c -> c -> c)
    -> Smash a b
    -> c
joinWith c f g k = joinSmash c k . bimap f g

-- -------------------------------------------------------------------- --
-- Partitioning

-- | Partition a list of 'Smash' values into a triple of lists of
-- all of their constituent parts
--
partitionAll :: [Smash a b] -> ([a], [b], [(a,b)])
partitionAll = flip foldr mempty $ \a ~(as, bs, cs) -> case a of
    Non -> (as, bs, cs)
    One a -> (a:as, bs, cs)
    Eno b -> (as, b:bs, cs)
    Two a b -> (as, bs, (a,b):cs)

-- | Partition a list of 'Smash' values into a tuple of lists of
-- their parts.
--
partition :: [Smash a b] -> ([a], [b])
partition = flip foldr mempty $ \a ~(as, bs) -> case a of
    Non -> (as, bs)
    One a -> (a:as, bs)
    Eno b -> (as, b:bs)
    Two a b -> (a:as, b:bs)

-- | Partition a list of 'Either' values, separating them into
-- a 'Smash' value of lists of left and right values, or 'Non' in the
-- case of an empty list.
--
partitionEithers :: [Either a b] -> Smash [a] [b]
partitionEithers = go . E.partitionEithers
  where
    go ([], []) = Non
    go (ls, []) = One ls
    go ([], rs) = Eno rs
    go (ls, rs) = Two ls rs

-- -------------------------------------------------------------------- --
-- Distributivity

-- | Distribute a 'Smash' value over a product.
--
distributeSmash :: Smash (a,b) c -> (Smash a c, Smash b c)
distributeSmash = \case
    Non -> (Non, Non)
    One (a,b) -> (One a, One b)
    Eno c -> (Eno c, Eno c)
    Two (a,b) c -> (Two a c, Two b c)

-- | Codistribute a coproduct over a 'Smash' value.
--
codistributeSmash :: Either (Smash a c) (Smash b c) -> Smash (Either a b) c
codistributeSmash = \case
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

reassocLR :: Smash (Smash a b) c -> Smash a (Smash b c)
reassocLR = \case
    Non -> Non
    One smash -> case smash of
      Non -> Eno Non
      One a -> One a
      Eno b -> Eno (One b)
      Two a b -> Two a (One b)
    Eno c -> Eno (Eno c)
    Two smash c -> case smash of
      Non -> Eno (Eno c)
      One a -> Two a (Eno c)
      Eno b -> Eno (Two b c)
      Two a b -> Two a (Two b c)

reassocRL :: Smash a (Smash b c) -> Smash (Smash a b) c
reassocRL = \case
    Non -> Non
    One a -> One (One a)
    Eno smash -> case smash of
      Non -> One Non
      One b -> One (Eno b)
      Eno c -> Eno c
      Two b c -> Two (Eno b) c
    Two a smash -> case smash of
      Non -> One (One a)
      One b -> One (Two a b)
      Eno c -> Two (One a) c
      Two b c -> Two (Two a b) c

-- -------------------------------------------------------------------- --
-- Symmetry

swapSmash :: Smash a b -> Smash b a
swapSmash = \case
    Non -> Non
    One a -> Eno a
    Eno b -> One b
    Two a b -> Two b a

-- -------------------------------------------------------------------- --
-- Std instances


instance (Hashable a, Hashable b) => Hashable (Smash a b)

instance Semigroup a => Applicative (Smash a) where
  pure = Eno

  _ <*> Non = Non
  Non <*> _ = Non
  One a <*> _ = One a
  Eno _ <*> One b = One b
  Eno f <*> Eno a = Eno (f a)
  Eno f <*> Two a b = Two a (f b)
  Two a f <*> One b = One (a <> b)
  Two a f <*> Eno b = Two a (f b)
  Two a f <*> Two b c = Two (a <> b) (f c)

instance Semigroup a => Monad (Smash a) where
  return = pure

  Non >>= _ = Non
  One a >>= k = One a
  Eno b >>= k = k b
  Two a b >>= k = case k b of
    Non -> Non
    One c -> One (a <> c)
    Eno c -> Eno c
    Two c d -> Two (a <> c) d

  (>>) = (*>)

instance (Semigroup a, Semigroup b) => Semigroup (Smash a b) where
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


instance (Semigroup a, Semigroup b) => Monoid (Smash a b) where
  mempty = Non
  mappend = (<>)

instance Functor (Smash a) where
  fmap _ Non = Non
  fmap _ (One a) = One a
  fmap f (Eno b) = Eno (f b)
  fmap f (Two a b) = Two a (f b)

instance Foldable (Smash a) where
  foldMap k (Eno b) = k b
  foldMap k (Two a b) = k b
  foldMap _ _ = mempty

instance Traversable (Smash a) where
  traverse k = \case
    Non -> pure Non
    One a -> pure (One a)
    Eno b -> Eno <$> k b
    Two a b -> Two a <$> k b

-- -------------------------------------------------------------------- --
-- Bifunctors

instance Bifunctor Smash where
  bimap f g = \case
    Non -> Non
    One a -> One (f a)
    Eno b -> Eno (g b)
    Two a b -> Two (f a) (g b)

instance Bifoldable Smash where
  bifoldMap f g = \case
    Non -> mempty
    One a -> f a
    Eno b -> g b
    Two a b -> f a <> g b

instance Bitraversable Smash where
  bitraverse f g = \case
    Non -> pure Non
    One a -> One <$> f a
    Eno b -> Eno <$> g b
    Two a b -> Two <$> f a <*> g b
