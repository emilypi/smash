{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Wedge.Aeson
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the Aeson instances for the 'Wedge' datatype.
--
module Data.Wedge.Aeson where


import Data.Aeson
import Data.Aeson.Encoding (pair, emptyObject_)
import qualified Data.Aeson.KeyMap as KM
import Data.Wedge (Wedge(..))


instance (ToJSON a, ToJSON b) => ToJSON (Wedge a b) 
instance (FromJSON a, FromJSON b) => FromJSON (Wedge a b) 


instance ToJSON2 Wedge where
    liftToJSON2 _ f _ _ _ _ (Here a) = object [ "Here" .= f a ]
    liftToJSON2 _ _ _ _ g _ (There b) = object [ "There" .= g b ]
    liftToJSON2 _ _ _ _ _ _ Nowhere = object []

    liftToEncoding2 _ f _ _ _ _ (Here a) = pairs $ pair "Here" (f a)
    liftToEncoding2 _ _ _ _ g _ (There b) = pairs $ pair "There" (g b)
    liftToEncoding2 _ _ _ _ _ _ Nowhere = emptyObject_


instance ToJSON a => ToJSON1 (Wedge a) 

instance FromJSON2 Wedge where
    liftParseJSON2 _ f _ _ g _ = withObject "Wedge a b" (go . KM.toList)
      where
        go [] = pure Nowhere
        go [("Here", a)] = Here <$> f a
        go [("There", b)] = There <$> g b
        go _  = fail "Expected either empty object, or one with 'Here' or 'There' keys only"

instance FromJSON a => FromJSON1 (Wedge a) 
