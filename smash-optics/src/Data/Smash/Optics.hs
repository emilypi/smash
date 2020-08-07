{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Smash.Optics
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Smash' datatype.
--
module Data.Smash.Optics
( -- * Prisms
  _Nada
, _Smash
   -- * Traversals
, smashed
, smashing
) where


import Optics.AffineTraversal
import Optics.Each.Core
import Optics.Iso
import Optics.IxTraversal
import Optics.Prism

import Data.Smash


-- ------------------------------------------------------------------- --
-- Traversals

-- | An 'AffineTraversal' of the smashed pair.
--
-- >>> over smashed (fmap pred) (Smash 1 2)
-- Smash 1 1
--
-- >>> over smashed id Nada
-- Nada
--
smashed :: AffineTraversal (Smash a b) (Smash c d) (a,b) (c,d)
smashed = atraversalVL $ \point f -> \case
  Nada -> point Nada
  Smash a b -> uncurry Smash <$> f (a,b)

-- | An 'IxTraversal' of the smashed pair. Yes this is equivalent to 'each'.
-- It's here because it's __smashing__.
--
-- >>> over smashing show (Smash 1 2)
-- Smash "1" "2"
--
-- >>> over smashing show Nada
-- Nada
--
smashing :: IxTraversal Bool (Smash a a) (Smash b b) a b
smashing = itraversalVL $ \f -> \case
  Nada -> pure Nada
  Smash a b -> Smash <$> f True a <*> f False b

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Prism'' selecting the 'Nada' constructor.
--
-- /Note:/ cannot change type.
--
_Nada :: Prism' (Smash a b) ()
_Nada = prism (const Nada) $ \case
  Nada -> Right ()
  Smash a b -> Left (Smash a b)

-- | A 'Prism'' selecting the 'Smash' constructor.
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

instance (a ~ a', b ~ b') => Each Bool (Smash a a') (Smash b b') a b where
  each = smashing
