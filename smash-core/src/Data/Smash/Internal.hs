{-# language Safe #-}
-- |
-- Module       : Data.Smash.Internal
-- Copyright    : (c) 2020-2022 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Asad Saeeduddin <https://github.com/masaeedu>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- This module contains utilities for distributing and codistributing
-- bifunctors over monoidal actions.
--
module Data.Smash.Internal
( unzipFirst
, undecideFirst
) where

import Data.Bifunctor

unzipFirst :: Bifunctor f => f (a, b) c -> (f a c, f b c)
unzipFirst fabc = (first fst fabc, first snd fabc)

undecideFirst :: Bifunctor f => Either (f a c) (f b c) -> f (Either a b) c
undecideFirst (Left fac) = first Left fac
undecideFirst (Right fbc) = first Right fbc
