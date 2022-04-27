{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Can.Lens
-- Copyright 	: (c) 2020-2022 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Can' datatype.
--
module Data.Can.Lens
( -- * Isos
  _CanIso
  -- * Prisms
, _Non
, _One
, _Eno
, _Two
  -- * Traversals
, oneing
, enoing
, twoed
, twoing
) where


import Control.Lens

import Data.Can


-- ------------------------------------------------------------------- --
-- Isos

-- | A 'Control.Lens.Iso' between a wedge coproduct and pointed coproduct.
--
_CanIso :: Iso (Can a b) (Can c d) (Maybe a, Maybe b) (Maybe c, Maybe d)
_CanIso = iso f g
  where
    f t = (canFst t, canSnd t)

    g (Nothing, Nothing) = Non
    g (Just a, Nothing) = One a
    g (Nothing, Just b) = Eno b
    g (Just a, Just b) = Two a b

-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Control.Lens.Traversal' of the first parameter, suitable for use
-- with "Control.Lens".
--
oneing :: Traversal (Can a c) (Can b c) a b
oneing f = \case
  Non -> pure Non
  One a -> One <$> f a
  Eno c -> pure (Eno c)
  Two a c -> flip Two c <$> f a

-- | A 'Control.Lens.Traversal' of the second parameter, suitable for use
-- with "Control.Lens".
--
enoing :: Traversal (Can a b) (Can a c) b c
enoing f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> Eno <$> f b
  Two a b -> Two a <$> f b

-- | A 'Control.Lens.Traversal' of the pair, suitable for use
-- with "Control.Lens".
--
twoed :: Traversal' (Can a b) (a,b)
twoed f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> pure (Eno b)
  Two a b -> uncurry Two <$> f (a,b)

-- | A 'Control.Lens.Traversal' of the pair ala 'both', suitable for use
-- with "Control.Lens".
--
twoing :: Traversal (Can a a) (Can b b) a b
twoing f = \case
  Non -> pure Non
  One a -> One <$> f a
  Eno a -> Eno <$> f a
  Two a b -> Two <$> f a <*> f b

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Control.Lens.Prism'' selecting the 'Non' constructor.
--
-- /Note:/ cannot change type.
--
_Non :: Prism' (Can a b) ()
_Non = prism (const Non) $ \case
  Non -> Right ()
  One a -> Left (One a)
  Eno b -> Left (Eno b)
  Two a b -> Left (Two a b)

-- | A 'Control.Lens.Prism'' selecting the 'One' constructor.
--
-- /Note:/ cannot change type.
--
_One :: Prism' (Can a b) a
_One = prism One $ \case
  Non -> Left Non
  One a -> Right a
  Eno b -> Left (Eno b)
  Two a b -> Left (Two a b)

-- | A 'Control.Lens.Prism'' selecting the 'Eno' constructor.
--
-- /Note:/ cannot change type.
--
_Eno :: Prism' (Can a b) b
_Eno = prism Eno $ \case
  Non -> Left Non
  One a -> Left (One a)
  Eno b -> Right b
  Two a b -> Left (Two a b)

-- | A 'Control.Lens.Prism'' selecting the 'Two' constructor.
--
-- /Note:/ cannot change type.
--
_Two :: Prism' (Can a b) (a,b)
_Two = prism (uncurry Two) $ \case
  Non -> Left Non
  One a -> Left (One a)
  Eno b -> Left (Eno b)
  Two a b -> Right (a,b)

-- ------------------------------------------------------------------- --
-- Orphans

#if ! MIN_VERSION_lens(5,0,0)
instance Swapped Can where
  swapped = iso swapCan swapCan
#endif

instance (a ~ a', b ~ b') => Each (Can a a') (Can b b') a b where
  each _ Non = pure Non
  each f (One a) = One <$> f a
  each f (Eno a) = Eno <$> f a
  each f (Two a b) = Two <$> f a <*> f b
