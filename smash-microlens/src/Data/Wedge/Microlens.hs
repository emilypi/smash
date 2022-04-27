{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Wedge.Microlens
-- Copyright 	: (c) 2020-2022 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: FlexibleInstances, MPTC, Type Families, UndecideableInstances
--
-- 'Prism's and 'Traversal's for the 'Wedge' datatype.
--
module Data.Wedge.Microlens
( -- * Traversals
  here
, there
, _Nowhere
, _Here
, _There
) where


import Lens.Micro

import Data.Wedge


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

-- | A 'Traversal'' selecting the 'Nowhere' constructor.
--
-- /Note:/ cannot change type.
--
_Nowhere :: Traversal' (Wedge a b) ()
_Nowhere f = \case
  Nowhere -> Nowhere <$ f ()
  Here a -> pure (Here a)
  There b -> pure (There b)

-- | A 'Traversal'' selecting the 'Here' constructor.
--
-- /Note:/ cannot change type.
--
_Here :: Traversal (Wedge a b) (Wedge c b) a c
_Here f = \case
  Here a -> Here <$> f a
  There b -> pure (There b)
  Nowhere -> pure Nowhere

-- | A 'Traversal'' selecting the 'There' constructor.
--
-- /Note:/ cannot change type.
--
_There :: Traversal (Wedge a b) (Wedge a d) b d
_There f = \case
  There b -> There <$> f b
  Here a -> pure (Here a)
  Nowhere -> pure Nowhere
