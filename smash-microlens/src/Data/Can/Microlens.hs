{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Can.Microlens
-- Copyright 	: (c) 2020-2021 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Can' datatype.
--
module Data.Can.Microlens
( -- * Traversals
  _Non
, _One
, _Eno
, _Two
, oneing
, enoing
, twoed
, twoing
) where


import Lens.Micro

import Data.Can


-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Traversal' of the first parameter, suitable for use
-- with "Control.Lens".
--
oneing :: Traversal (Can a c) (Can b c) a b
oneing f = \case
  Non -> pure Non
  One a -> One <$> f a
  Eno c -> pure (Eno c)
  Two a c -> flip Two c <$> f a

-- | A 'Traversal' of the second parameter, suitable for use
-- with "Control.Lens".
--
enoing :: Traversal (Can a b) (Can a c) b c
enoing f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> Eno <$> f b
  Two a b -> Two a <$> f b

-- | A 'Traversal' of the pair, suitable for use
-- with "Control.Lens".
--
twoed :: Traversal' (Can a b) (a,b)
twoed f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> pure (Eno b)
  Two a b -> uncurry Two <$> f (a,b)

-- | A 'Traversal' of the pair ala 'both', suitable for use
-- with "Control.Lens".
--
twoing :: Traversal (Can a a) (Can b b) a b
twoing f = \case
  Non -> pure Non
  One a -> One <$> f a
  Eno a -> Eno <$> f a
  Two a b -> Two <$> f a <*> f b

-- | A 'Traversal'' selecting the 'Non' constructor.
--
-- /Note:/ cannot change type.
--
_Non :: Traversal' (Can a b) ()
_Non f = \case
  Non -> Non <$ f ()
  One a -> pure (One a)
  Eno b -> pure (Eno b)
  Two a b -> pure (Two a b)

-- | A 'Traversal'' selecting the 'One' constructor.
--
-- /Note:/ cannot change type.
--
_One :: Traversal' (Can a b) a
_One f = \case
  Non -> pure Non
  One a -> One <$> f a
  Eno b -> pure (Eno b)
  Two a b -> pure (Two a b)

-- | A 'Traversal'' selecting the 'Eno' constructor.
--
-- /Note:/ cannot change type.
--
_Eno :: Traversal' (Can a b) b
_Eno f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> Eno <$> f b
  Two a b -> pure (Two a b)

-- | A 'Traversal'' selecting the 'Two' constructor.
--
-- /Note:/ cannot change type.
--
_Two :: Traversal' (Can a b) (a,b)
_Two f = \case
  Non -> pure Non
  One a -> pure (One a)
  Eno b -> pure (Eno b)
  Two a b -> uncurry Two <$> f (a,b)
