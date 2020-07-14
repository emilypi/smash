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


import Optics.Prism
import Optics.Traversal

import Data.Wedge


-- ------------------------------------------------------------------- --
-- Traversals

-- | A 'Optics.Traversal' of the 'Here' case of a 'Wedge',
-- suitable for use with "Optics".
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

-- | A 'Optics.Traversal' of the 'There' case of a 'Wedge',
-- suitable for use with "Optics".
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

-- | A 'Optics.Prism'' selecting the 'Nowhere' constructor.
--
-- /Note:/ cannot change type.
--
_Nowhere :: Prism' (Wedge a b) ()
_Nowhere = prism (const Nowhere) $ \case
  Nowhere -> Right ()
  Here a -> Left (Here a)
  There b -> Left (There b)

-- | A 'Optics.Prism'' selecting the 'Here' constructor.
--
-- /Note:/ cannot change type.
--
_Here :: Prism (Wedge a b) (Wedge c b) a c
_Here = prism Here $ \case
  Here a -> Right a
  There b -> Left (There b)
  Nowhere -> Left Nowhere

-- | A 'Optics.Prism'' selecting the 'There' constructor.
--
-- /Note:/ cannot change type.
--
_There :: Prism (Wedge a b) (Wedge a d) b d
_There = prism There $ \case
  There b -> Right b
  Here a -> Left (Here a)
  Nowhere -> Left (Nowhere)
