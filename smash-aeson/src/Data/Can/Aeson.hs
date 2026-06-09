{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Data.Can.Aeson
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-3-Clause
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : CPP
--
-- This module contains the Aeson instances for the 'Can' datatype.
--
module Data.Can.Aeson where


import Data.Aeson
import Data.Aeson.Encoding (emptyObject_, pair)
import qualified Data.Aeson.KeyMap as KM
import Data.Can (Can(..))


instance (ToJSON a, ToJSON b) => ToJSON (Can a b) 
instance (FromJSON a, FromJSON b) => FromJSON (Can a b) 


instance ToJSON2 Can where
    liftToJSON2 _ f _ _ _ _ (One a) = object [ "One" .= f a ]
    liftToJSON2 _ _ _ _ g _ (Eno b) = object [ "Eno" .= g b ]
    liftToJSON2 _ f _ _ g _ (Two a b) = object [ "One" .= f a, "Eno" .= g b ]
    liftToJSON2 _ _ _ _ _ _ Non = object []

    liftToEncoding2 _ f _ _ _ _ (One a) = pairs $ pair "One" (f a)
    liftToEncoding2 _ _ _ _ g _ (Eno b) = pairs $ pair "Eno" (g b)
    liftToEncoding2 _ f _ _ g _ (Two a b) = pairs $ pair "One" (f a) <> pair "Eno" (g b)
    liftToEncoding2 _ _ _ _ _ _ Non = emptyObject_


instance ToJSON a => ToJSON1 (Can a) 

instance FromJSON2 Can where
    liftParseJSON2 _ f _ _ g _ = withObject "Can a b" (go . KM.toList)
      where
        go [] = pure Non
        go [("One", a)] = One <$> f a
        go [("Eno", b)] = Eno <$> g b
        go [("One", a), ("Eno", b)] = Two <$> f a <*> g b
        go _  = fail "Expected either empty object, or 'One' and 'Eno' keys, 'One' or 'Eno' keys only"

instance FromJSON a => FromJSON1 (Can a) 
