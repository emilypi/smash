{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Can.Optics
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Can' datatype.
--
module Data.Can.Optics
( -- * Prisms
  _Non
, _One
, _Eno
, _Two
  -- * Traversals
, oneing
, enoing
, twoed
, twoing
) where


import Data.Can

import Optics.Each.Core
import Optics.Iso
import Optics.IxTraversal
import Optics.Prism
import Optics.Traversal

-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Optics.Traversal' of the first parameter, suitable for use
-- with "Optics".
--
oneing :: Traversal (Can a c) (Can b c) a b
oneing = traversalVL $ \f -> \case
  Non -> pure Non
  One a -> One <$> f a
  Eno c -> pure (Eno c)
  Two a c -> flip Two c <$> f a

-- | A 'Optics.Traversal' of the second parameter, suitable for use
-- with "Optics".
--
enoing :: Traversal (Can a b) (Can a c) b c
enoing = traversalVL $ \f -> \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> Eno <$> f b
  Two a b -> Two a <$> f b

-- | A 'Optics.Traversal' of the pair, suitable for use
-- with "Optics".
--
-- /Note:/ cannot change type.
--
twoed :: Traversal' (Can a b) (a,b)
twoed = traversalVL $ \f -> \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> pure (Eno b)
  Two a b -> uncurry Two <$> f (a,b)

-- | A 'Optics.Traversal' of the pair ala 'both', suitable for use
-- with "Optics".
--
twoing :: Traversal (Can a a) (Can b b) a b
twoing = traversalVL $ \f -> \case
  Non -> pure Non
  One a -> One <$> f a
  Eno a -> Eno <$> f a
  Two a b -> Two <$> f a <*> f b

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Optics.Prism'' selecting the 'Non' constructor.
--
-- /Note:/ cannot change type.
--
_Non :: Prism' (Can a b) ()
_Non = prism (const Non) $ \case
  Non -> Right ()
  One a -> Left (One a)
  Eno b -> Left (Eno b)
  Two a b -> Left (Two a b)

-- | A 'Optics.Prism'' selecting the 'One' constructor.
--
-- /Note:/ cannot change type.
--
_One :: Prism' (Can a b) a
_One = prism One $ \case
  Non -> Left Non
  One a -> Right a
  Eno b -> Left (Eno b)
  Two a b -> Left (Two a b)

-- | A 'Optics.Prism'' selecting the 'Eno' constructor.
--
-- /Note:/ cannot change type.
--
_Eno :: Prism' (Can a b) b
_Eno = prism Eno $ \case
  Non -> Left Non
  One a -> Left (One a)
  Eno b -> Right b
  Two a b -> Left (Two a b)

-- | A 'Optics.Prism'' selecting the 'Two' constructor.
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

instance Swapped Can where
  swapped = iso swapCan swapCan

instance (a ~ a', b ~ b') => Each (Maybe Bool) (Can a a') (Can b b') a b where
  each = itraversalVL $ \f -> \case
    Non -> pure Non
    One a -> One <$> f (Just True) a
    Eno a -> Eno <$> f (Just False) a
    Two a b -> Two <$> f (Just True) a <*> f (Just False) b
