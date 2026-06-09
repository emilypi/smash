{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Smash.Aeson
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : CPP
--
-- This module contains the Aeson instances for the 'Smash' datatype.
-- The 'Smash' instances,  explicitly naming the tuple entries using `SmashL` and `SmashR`
--
module Data.Smash.Aeson where


import Data.Aeson
import Data.Aeson.Encoding (emptyObject_, pair)
import qualified Data.Aeson.KeyMap as KM
import Data.Smash (Smash(..))


instance (ToJSON a, ToJSON b) => ToJSON (Smash a b) 

instance (FromJSON a, FromJSON b) => FromJSON (Smash a b) 


instance ToJSON2 Smash where
    liftToJSON2 _ f _ _ g _ (Smash a b) = object [ "SmashL" .= f a, "SmashR" .= g b ]
    liftToJSON2 _ _ _ _ _ _ Nada = object []

    liftToEncoding2 _ f _ _ g _ (Smash a b) = pairs $ pair "SmashL" (f a) <> pair "SmashR" (g b)
    liftToEncoding2 _ _ _ _ _ _ Nada = emptyObject_


instance ToJSON a => ToJSON1 (Smash a) 
instance FromJSON2 Smash where
    liftParseJSON2 _ f _ _ g _ = withObject "Smash a b" (go . KM.toList)
      where
        go [] = pure Nada
        go [("SmashL", a), ("SmashR", b)] = Smash <$> f a <*> g b
        go _ = fail "Expected either empty object, or a 'Smash' pair"

instance FromJSON a => FromJSON1 (Smash a)
