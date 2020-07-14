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


import Optics.Prism
import Optics.Traversal

import Data.Smash

-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Optics.Traversal' of the smashed pair, suitable for use
-- with "Optics".
--
-- >>> over smashed show (Smash 1 2)
-- "(1,2)"
--
-- >>> over smashed show Nada
-- Nada
--
smashed :: Traversal (Smash a b) (Smash c d) (a,b) (c,d)
smashed f = \case
  Nada -> pure Nada
  Smash a b -> uncurry Smash <$> f (a,b)

-- | A 'Optics.Traversal' of the smashed pair, suitable for use
-- with "Optics".
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

-- | A 'Optics.Prism'' selecting the 'Nada' constructor.
--
-- /Note:/ cannot change type.
--
_Nada :: Prism' (Smash a b) ()
_Nada = prism (const Nada) $ \case
  Nada -> Right ()
  Smash a b -> Left (Smash a b)

-- | A 'Optics.Prism'' selecting the 'Smash' constructor.
--
-- /Note:/ cannot change type.
--
_Smash :: Prism' (Smash a b) (a,b)
_Smash = prism (uncurry Smash) $ \case
  Smash a b -> Right (a,b)
  Nada -> Left Nada
