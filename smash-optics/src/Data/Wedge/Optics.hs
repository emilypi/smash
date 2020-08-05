{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Wedge.Optics
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Wedge' datatype.
--
module Data.Wedge.Optics
( -- * Traversals
  here
, there
  -- * Prisms
, _Nowhere
, _Here
, _There
) where


import Data.Wedge

import Optics.AffineTraversal
import Optics.Each.Core
import Optics.Iso
import Optics.IxTraversal
import Optics.Prism


-- ------------------------------------------------------------------- --
-- Traversals

-- | An 'AffineTraversal' of the 'Here' case of a 'Wedge',
-- suitable for use with "Optics".
--
-- >>> over here show (Here 1)
-- Here "1"
--
-- >>> over here show (There 'a')
-- There 'a'
--
here :: AffineTraversal (Wedge a b) (Wedge a' b) a a'
here = atraversalVL $ \point f -> \case
  Nowhere -> point Nowhere
  Here a -> Here <$> f a
  There b -> point (There b)

-- | An 'AffineTraversal' of the 'There' case of a 'Wedge',
-- suitable for use with "Optics".
--
-- >>> over there show (Here 1)
-- Here 1
--
-- >>> over there show (There 'a')
-- There "'a'"
--
there :: AffineTraversal (Wedge a b) (Wedge a b') b b'
there = atraversalVL $ \point f -> \case
  Nowhere -> point Nowhere
  Here a -> point (Here a)
  There b -> There <$> f b

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Prism'' selecting the 'Nowhere' constructor.
--
-- /Note:/ this optic cannot change type.
--
_Nowhere :: Prism' (Wedge a b) ()
_Nowhere = prism (const Nowhere) $ \case
  Nowhere -> Right ()
  Here a -> Left (Here a)
  There b -> Left (There b)

-- | A 'Prism'' selecting the 'Here' constructor.
--
_Here :: Prism (Wedge a b) (Wedge c b) a c
_Here = prism Here $ \case
  Here a -> Right a
  There b -> Left (There b)
  Nowhere -> Left Nowhere

-- | A 'Prism'' selecting the 'There' constructor.
--
_There :: Prism (Wedge a b) (Wedge a d) b d
_There = prism There $ \case
  There b -> Right b
  Here a -> Left (Here a)
  Nowhere -> Left (Nowhere)

-- ------------------------------------------------------------------- --
-- Orphans

instance Swapped Wedge where
  swapped = iso swapWedge swapWedge

instance (a ~ a', b ~ b') => Each (Maybe Bool) (Wedge a a') (Wedge b b') a b where
  each = itraversalVL $ \f -> \case
    Here a -> Here <$> f (Just True) a
    There b -> There <$> f (Just False) b
    Nowhere -> pure Nowhere
