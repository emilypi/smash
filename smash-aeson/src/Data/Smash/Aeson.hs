{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Smash.Aeson
-- Copyright    : (c) 2020 Emily Pillmore
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
#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup(..))
#endif
import Data.Smash (Smash(..))


instance (ToJSON a, ToJSON b) => ToJSON (Smash a b) where
    toJSON Nada = object []
    toJSON (Smash a b) = object [ "SmashL" .= a, "SmashR" .= b ]

    toEncoding (Smash a b) = pairs $ "SmashL" .= a <> "SmashR" .= b
    toEncoding Nada = emptyObject_

instance (FromJSON a, FromJSON b) => FromJSON (Smash a b) where
    parseJSON = withObject "Smash a b" (go . KM.toList)
      where
        go [("SmashL", a), ("SmashR", b)] = Smash <$> parseJSON a <*> parseJSON b
        go [] = pure Nada
        go _  = fail "Expected either empty object, or a 'Smash' pair"


instance ToJSON2 Smash where
    liftToJSON2 f _ g _ (Smash a b) = object [ "SmashL" .= f a, "SmashR" .= g b ]
    liftToJSON2 _ _ _ _ Nada = object []

    liftToEncoding2 f _ g _ (Smash a b) = pairs $ pair "SmashL" (f a) <> pair "SmashR" (g b)
    liftToEncoding2 _ _ _ _ Nada = emptyObject_


instance ToJSON a => ToJSON1 (Smash a) where
    liftToJSON g _ (Smash a b) = object [ "SmashL" .= a, "SmashR" .= g b ]
    liftToJSON _ _ Nada = object []

    liftToEncoding g _ (Smash a b) = pairs $ "SmashL" .= a <> pair "SmashR" (g b)
    liftToEncoding _ _ Nada = emptyObject_

instance FromJSON2 Smash where
    liftParseJSON2 f _ g _ = withObject "Smash a b" (go . KM.toList)
      where
        go [] = pure Nada
        go [("SmashL", a), ("SmashR", b)] = Smash <$> f a <*> g b
        go _ = fail "Expected either empty object, or a 'Smash' pair"

instance FromJSON a => FromJSON1 (Smash a) where
    liftParseJSON f _ = withObject "Smash a b" (go . KM.toList)
      where
        go [] = pure Nada
        go [("SmashL", a), ("SmashR", b)] = Smash <$> parseJSON a <*> f b
        go _ = fail "Expected either empty object, or a 'Smash' pair"
