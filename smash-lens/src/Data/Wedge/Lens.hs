{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Wedge.Lens
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Wedge' datatype.
--
module Data.Wedge.Lens
( -- * Isos
  _WedgeIso
  -- * Traversals
, here
, there
  -- * Prisms
, _Nowhere
, _Here
, _There
) where


import Control.Lens

import Data.Wedge

-- ------------------------------------------------------------------- --
-- Isos

-- | A 'Control.Lens.Iso' between a wedge sum and pointed coproduct.
--
_WedgeIso :: Iso (Wedge a b) (Wedge c d) (Maybe (Either a b)) (Maybe (Either c d))
_WedgeIso = iso f g
  where
    f Nowhere = Nothing
    f (Here a) = Just (Left a)
    f (There b) = Just (Right b)

    g Nothing = Nowhere
    g (Just (Left a)) = Here a
    g (Just (Right b)) = There b

-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Control.Lens.Traversal' of the 'Here' case of a 'Wedge',
-- suitable for use with "Control.Lens".
--
-- >>> over here show (Here 1)
-- Here "1"
--
-- >>> over here show (There 'a')
-- There 'a'
--
here :: Traversal' (Wedge a b) a
here f = \case
  Nowhere -> pure Nowhere
  Here a -> Here <$> f a
  There b -> pure (There b)

-- | A 'Control.Lens.Traversal' of the 'There' case of a 'Wedge',
-- suitable for use with "Control.Lens".
--
-- >>> over there show (Here 1)
-- Here 1
--
-- >>> over there show (There 'a')
-- There "'a'"
--
there :: Traversal' (Wedge a b) b
there f = \case
  Nowhere -> pure Nowhere
  Here a -> pure (Here a)
  There b -> There <$> f b

-- ------------------------------------------------------------------- --
-- Prisms

-- | A 'Control.Lens.Prism'' selecting the 'Nowhere' constructor.
--
-- /Note:/ cannot change type.
--
_Nowhere :: Prism' (Wedge a b) ()
_Nowhere = prism (const Nowhere) $ \case
  Nowhere -> Right ()
  Here a -> Left (Here a)
  There b -> Left (There b)

-- | A 'Control.Lens.Prism'' selecting the 'Here' constructor.
--
-- /Note:/ cannot change type.
--
_Here :: Prism (Wedge a b) (Wedge c b) a c
_Here = prism Here $ \case
  Here a -> Right a
  There b -> Left (There b)
  Nowhere -> Left Nowhere

-- | A 'Control.Lens.Prism'' selecting the 'There' constructor.
--
-- /Note:/ cannot change type.
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

instance (a ~ a', b ~ b') => Each (Wedge a a') (Wedge b b') a b where
  each _ Nowhere = pure Nowhere
  each f (Here a) = Here <$> f a
  each f (There a) = There <$> f a
