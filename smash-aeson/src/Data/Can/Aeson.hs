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
module Data.Can.Aeson () where


import Data.Aeson
import Data.Aeson.Encoding (emptyObject_, pair)
import qualified Data.Aeson.KeyMap as KM
import Data.Can (Can(..))


instance (ToJSON a, ToJSON b) => ToJSON (Can a b) where
    toJSON Non = object []
    toJSON (One a) = object [ "One" .= a ]
    toJSON (Eno b) = object [ "Eno" .= b ]
    toJSON (Two a b) = object [ "One" .= a, "Eno" .= b ]

    toEncoding (One a) = pairs $ "One" .= a
    toEncoding (Eno b) = pairs $ "Eno" .= b
    toEncoding (Two a b) = pairs $ "One" .= a <> "Eno" .= b
    toEncoding Non = emptyObject_

instance (FromJSON a, FromJSON b) => FromJSON (Can a b) where
    parseJSON = withObject "Can a b" (go . KM.toAscList)
      where
        go [("One", a)] = One <$> parseJSON a
        go [("Eno", b)] = Eno <$> parseJSON b
        go [("Eno", b), ("One", a)] = Two <$> parseJSON a <*> parseJSON b
        go [] = pure Non
        go _  = fail "Expected either empty object, or 'One' and 'Eno' keys, 'One' or 'Eno' keys only"


instance ToJSON2 Can where
#if MIN_VERSION_aeson(2,2,0)
    liftToJSON2 _ f _ _ _ _ (One a) = object [ "One" .= f a ]
    liftToJSON2 _ _ _ _ g _ (Eno b) = object [ "Eno" .= g b ]
    liftToJSON2 _ f _ _ g _ (Two a b) = object [ "One" .= f a, "Eno" .= g b ]
    liftToJSON2 _ _ _ _ _ _ Non = object []
#else
    liftToJSON2 f _ _ _ (One a) = object [ "One" .= f a ]
    liftToJSON2 _ _ g _ (Eno b) = object [ "Eno" .= g b ]
    liftToJSON2 f _ g _ (Two a b) = object [ "One" .= f a, "Eno" .= g b ]
    liftToJSON2 _ _ _ _ Non = object []
#endif

#if MIN_VERSION_aeson(2,2,0)
    liftToEncoding2 _ f _ _ _ _ (One a) = pairs $ pair "One" (f a)
    liftToEncoding2 _ _ _ _ g _ (Eno b) = pairs $ pair "Eno" (g b)
    liftToEncoding2 _ f _ _ g _ (Two a b) = pairs $ pair "One" (f a) <> pair "Eno" (g b)
    liftToEncoding2 _ _ _ _ _ _ Non = emptyObject_
#else
    liftToEncoding2 f _ _ _ (One a) = pairs $ pair "One" (f a)
    liftToEncoding2 _ _ g _ (Eno b) = pairs $ pair "Eno" (g b)
    liftToEncoding2 f _ g _ (Two a b) = pairs $ pair "One" (f a) <> pair "Eno" (g b)
    liftToEncoding2 _ _ _ _ Non = emptyObject_
#endif


instance ToJSON a => ToJSON1 (Can a) where
#if MIN_VERSION_aeson(2,2,0)
    liftToJSON _ _ _ (One a) = object [ "One" .= a ]
    liftToJSON _ g _ (Eno b) = object [ "Eno" .= g b ]
    liftToJSON _ g _ (Two a b) = object [ "One" .= a, "Eno" .= g b ]
    liftToJSON _ _ _ Non = object []
#else
    liftToJSON _ _ (One a) = object [ "One" .= a ]
    liftToJSON g _ (Eno b) = object [ "Eno" .= g b ]
    liftToJSON g _ (Two a b) = object [ "One" .= a, "Eno" .= g b ]
    liftToJSON _ _ Non = object []
#endif

#if MIN_VERSION_aeson(2,2,0)
    liftToEncoding _ _ _ (One a) = pairs $ "One" .= a
    liftToEncoding _ g _ (Eno b) = pairs $ pair "Eno" (g b)
    liftToEncoding _ g _ (Two a b) = pairs $ "One" .= a <> pair "Eno" (g b)
    liftToEncoding _ _ _ Non = emptyObject_
#else
    liftToEncoding _ _ (One a) = pairs $ "One" .= a
    liftToEncoding g _ (Eno b) = pairs $ pair "Eno" (g b)
    liftToEncoding g _ (Two a b) = pairs $ "One" .= a <> pair "Eno" (g b)
    liftToEncoding _ _ Non = emptyObject_
#endif

instance FromJSON2 Can where
#if MIN_VERSION_aeson(2,2,0)
    liftParseJSON2 _ f _ _ g _ = withObject "Can a b" (go . KM.toAscList)
#else
    liftParseJSON2 f _ g _ = withObject "Can a b" (go . KM.toAscList)
#endif
      where
        go [] = pure Non
        go [("One", a)] = One <$> f a
        go [("Eno", b)] = Eno <$> g b
        go [("Eno", b), ("One", a)] = Two <$> f a <*> g b
        go _  = fail "Expected either empty object, or 'One' and 'Eno' keys, 'One' or 'Eno' keys only"

instance FromJSON a => FromJSON1 (Can a) where
#if MIN_VERSION_aeson(2,2,0)
    liftParseJSON _ f _ = withObject "Can a b" (go . KM.toAscList)
#else
    liftParseJSON f _ = withObject "Can a b" (go . KM.toAscList)
#endif
      where
        go [] = pure Non
        go [("One", a)] = One <$> parseJSON a
        go [("Eno", b)] = Eno <$> f b
        go [("Eno", b), ("One", a)] = Two <$> parseJSON a <*> f b
        go _  = fail "Expected either empty object, or 'One' and 'Eno' keys, 'One' or 'Eno' keys only"
