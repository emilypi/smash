{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Smash.Lens
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Smash' datatype.
--
module Data.Smash.Lens
( -- * Prisms
  _Nada
, _Smash
   -- * Traversals
, smashed
, smashing
) where


import Control.Lens

import Data.Smash

-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Control.Lens.Traversal' of the smashed pair, suitable for use
-- with "Control.Lens".
--
-- >>> over smashed (fmap pred) (Smash 1 2)
-- Smash 1 1
--
-- >>> over smashed id Nada
-- Nada
--
smashed :: Traversal (Smash a b) (Smash c d) (a,b) (c,d)
smashed f = \case
  Nada -> pure Nada
  Smash a b -> uncurry Smash <$> f (a,b)

-- | A 'Control.Lens.Traversal' of the smashed pair, suitable for use
-- with "Control.Lens".
--
-- >>> over smashing show (Smash 1 2)
-- Smash "1" "2"
--
-- >>> over smashing show Nada
-- Nada
--
smashing :: Traversal (Smash a a) (Smash b b) a b
smashing = smashed . both

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Control.Lens.Prism'' selecting the 'Nada' constructor.
--
-- /Note:/ cannot change type.
--
_Nada :: Prism' (Smash a b) ()
_Nada = prism (const Nada) $ \case
  Nada -> Right ()
  Smash a b -> Left (Smash a b)

-- | A 'Control.Lens.Prism'' selecting the 'Smash' constructor.
--
-- /Note:/ cannot change type.
--
_Smash :: Prism' (Smash a b) (a,b)
_Smash = prism (uncurry Smash) $ \case
  Smash a b -> Right (a,b)
  Nada -> Left Nada


-- ------------------------------------------------------------------- --
-- Orphans

instance Swapped Smash where
  swapped = iso swapSmash swapSmash

instance (a ~ a', b ~ b') => Each (Smash a a') (Smash b b') a b where
  each _ Nada = pure Nada
  each f (Smash a b) = Smash <$> f a <*> f b
