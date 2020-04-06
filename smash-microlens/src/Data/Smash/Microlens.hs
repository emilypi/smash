{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Smash.Microlens
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Traversal's for the 'Smash' datatype.
--
module Data.Smash.Microlens
( -- * Traversals
  _Nada
, _Smash
, smashed
, smashing
) where


import Lens.Micro

import Data.Smash


-- | A 'Traversal' of the smashed pair, suitable for use
-- with "Control.Lens".
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

-- | A 'Traversal' of the smashed pair, suitable for use
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

-- | A 'Traversal'' selecting the 'Nada' constructor.
--
-- /Note:/ cannot change type.
--
_Nada :: Traversal' (Smash a b) ()
_Nada f = \case
  Nada -> Nada <$ f ()
  Smash a b -> pure (Smash a b)

-- | A 'Traversal'' selecting the 'Smash' constructor.
--
-- /Note:/ cannot change type.
--
_Smash :: Traversal' (Smash a b) (a,b)
_Smash f = \case
  Smash a b -> uncurry Smash <$> f (a,b)
  Nada -> pure Nada
