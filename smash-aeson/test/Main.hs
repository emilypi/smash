{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) 2020-2026 Emily Pillmore
module Main (main) where

import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (Bifunctor, bimap)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Can
import Data.Can.Aeson ()
import Data.Either (isLeft)
import Data.Smash
import Data.Smash.Aeson ()
import Data.Wedge
import Data.Wedge.Aeson ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "smash-aeson"
  [ wireTests "Can"
      [(Non, "{}"), (One 7, "{\"One\":7}"), (Eno 9, "{\"Eno\":9}"),
       (Two 7 9, "{\"One\":7,\"Eno\":9}")]
      (oneof [pure Non, One <$> arbitrary, Eno <$> arbitrary, Two <$> arbitrary <*> arbitrary])
      ["{\"Other\":1}", "{\"One\":1,\"Other\":2}", "{\"One\":1,\"Eno\":2,\"Other\":3}",
       "{\"One\":null}", "{\"Eno\":false}", "{\"tag\":\"Non\"}"]
  , wireTests "Smash"
      [(Nada, "{}"), (Smash 7 9, "{\"SmashL\":7,\"SmashR\":9}")]
      (oneof [pure Nada, Smash <$> arbitrary <*> arbitrary])
      ["{\"SmashL\":1}", "{\"SmashR\":2}", "{\"SmashL\":1,\"SmashR\":2,\"Other\":3}",
       "{\"SmashL\":null,\"SmashR\":2}", "{\"tag\":\"Nada\"}"]
  , wireTests "Wedge"
      [(Nowhere, "{}"), (Here 7, "{\"Here\":7}"), (There 9, "{\"There\":9}")]
      (oneof [pure Nowhere, Here <$> arbitrary, There <$> arbitrary])
      ["{\"Here\":1,\"There\":2}", "{\"Other\":1}", "{\"Here\":1,\"Other\":2}",
       "{\"Here\":null}", "{\"There\":false}", "{\"tag\":\"Nowhere\"}"]
  , testCase "Can accepts both object key orders" $ forM_
      ["{\"One\":7,\"Eno\":9}", "{\"Eno\":9,\"One\":7}"] $ \bytes ->
        checkParsers (Two 7 9 :: Can Int Int) bytes
  , testCase "Smash accepts both object key orders" $ forM_
      ["{\"SmashL\":7,\"SmashR\":9}", "{\"SmashR\":9,\"SmashL\":7}"] $ \bytes ->
        checkParsers (Smash 7 9 :: Smash Int Int) bytes
  , testCase "nullable payloads retain their constructor keys" $ do
      checkNullable (Two Nothing Nothing :: Can (Maybe Int) (Maybe Int)) "{\"One\":null,\"Eno\":null}"
      checkNullable (One Nothing :: Can (Maybe Int) (Maybe Int)) "{\"One\":null}"
      checkNullable (Eno Nothing :: Can (Maybe Int) (Maybe Int)) "{\"Eno\":null}"
      checkNullable (Smash Nothing Nothing :: Smash (Maybe Int) (Maybe Int)) "{\"SmashL\":null,\"SmashR\":null}"
      checkNullable (Here Nothing :: Wedge (Maybe Int) (Maybe Int)) "{\"Here\":null}"
      checkNullable (There Nothing :: Wedge (Maybe Int) (Maybe Int)) "{\"There\":null}"
  ]

wireTests
  :: forall f. (Eq (f Int Int), Show (f Int Int), ToJSON (f Int Int), FromJSON (f Int Int),
                ToJSON1 (f Int), FromJSON1 (f Int), ToJSON2 f, FromJSON2 f, Bifunctor f)
  => String -> [(f Int Int, BL.ByteString)] -> Gen (f Int Int) -> [BL.ByteString] -> TestTree
wireTests name vectors gen invalid = testGroup name
  [ testGroup "Legacy wire fixtures"
      [ testCase (show x) $ do
          encode x @?= bytes
          toJSON x @?= fixtureValue bytes
          toJSON1 x @?= fixtureValue bytes
          toJSON2 x @?= fixtureValue bytes
          encodingToLazyByteString (toEncoding1 x) @?= bytes
          encodingToLazyByteString (toEncoding2 x) @?= bytes
          checkParsers x bytes
      | (x, bytes) <- vectors
      ]
  , testGroup "Reject malformed representations"
      [ testCase (BL.unpack bytes) $ do
          assertBool "ordinary parser accepted invalid input" (isLeft (eitherDecode bytes :: Either String (f Int Int)))
          assertBool "unary parser accepted invalid input" (isLeft (parseEither parseJSON1 (fixtureValue bytes) :: Either String (f Int Int)))
          assertBool "binary parser accepted invalid input" (isLeft (parseEither parseJSON2 (fixtureValue bytes) :: Either String (f Int Int)))
      | bytes <- ["null", "[]", "7", "true", "\"Non\""] ++ invalid
      ]
  , testProperty "ordinary and lifted roundtrips" $ forAll gen $ \x -> conjoin
      [ eitherDecode (encode x) === Right x
      , parseEither parseJSON1 (toJSON1 x) === Right x
      , parseEither parseJSON2 (toJSON2 x) === Right x
      , eitherDecode (encodingToLazyByteString (toEncoding1 x)) === Right x
      , eitherDecode (encodingToLazyByteString (toEncoding2 x)) === Right x
      ]
  , testProperty "lifted callbacks preserve keys and transform payloads" $ forAll gen $ \x -> conjoin
      [ customJSON1 x === toJSON (fmapRight x)
      , customJSON2 x === toJSON (bimap (+10) (*3) x)
      , fixtureValue (encodingToLazyByteString (customEncoding1 x)) === customJSON1 x
      , fixtureValue (encodingToLazyByteString (customEncoding2 x)) === customJSON2 x
      , parseEither customParse1 (customJSON1 x) === Right x
      , parseEither customParse2 (customJSON2 x) === Right x
      ]
  ]
  where
    fmapRight = bimap id (*3)

fixtureValue :: BL.ByteString -> Value
fixtureValue bytes = either error id (eitherDecode bytes)

checkParsers
  :: (Eq (f Int Int), Show (f Int Int), FromJSON (f Int Int), FromJSON1 (f Int), FromJSON2 f)
  => f Int Int -> BL.ByteString -> Assertion
checkParsers x bytes = do
  eitherDecode bytes @?= Right x
  parseEither parseJSON1 (fixtureValue bytes) @?= Right x
  parseEither parseJSON2 (fixtureValue bytes) @?= Right x

checkNullable
  :: (Eq (f (Maybe Int) (Maybe Int)), Show (f (Maybe Int) (Maybe Int)),
      ToJSON (f (Maybe Int) (Maybe Int)), FromJSON (f (Maybe Int) (Maybe Int)),
      ToJSON1 (f (Maybe Int)), FromJSON1 (f (Maybe Int)), ToJSON2 f, FromJSON2 f)
  => f (Maybe Int) (Maybe Int) -> BL.ByteString -> Assertion
checkNullable x bytes = do
  encode x @?= bytes
  toJSON1 x @?= fixtureValue bytes
  toJSON2 x @?= fixtureValue bytes
  encodingToLazyByteString (toEncoding1 x) @?= bytes
  encodingToLazyByteString (toEncoding2 x) @?= bytes
  eitherDecode bytes @?= Right x
  parseEither parseJSON1 (fixtureValue bytes) @?= Right x
  parseEither parseJSON2 (fixtureValue bytes) @?= Right x

-- Aeson 2.2 added omission arguments. Explicit constructor fields must still
-- be emitted and required even when callbacks would allow their omission.
#if MIN_VERSION_aeson(2,2,0)
#define OMIT (const True)
#define MISSING (Just 42)
#else
#define OMIT
#define MISSING
#endif

customJSON1 :: ToJSON1 f => f Int -> Value
customJSON1 = liftToJSON OMIT (toJSON . (*3)) toJSON

customJSON2 :: ToJSON2 f => f Int Int -> Value
customJSON2 = liftToJSON2 OMIT (toJSON . (+10)) toJSON OMIT (toJSON . (*3)) toJSON

customEncoding1 :: ToJSON1 f => f Int -> Encoding
customEncoding1 = liftToEncoding OMIT (toEncoding . (*3)) toEncoding

customEncoding2 :: ToJSON2 f => f Int Int -> Encoding
customEncoding2 = liftToEncoding2 OMIT (toEncoding . (+10)) toEncoding OMIT (toEncoding . (*3)) toEncoding

customParse1 :: FromJSON1 f => Value -> Parser (f Int)
customParse1 = liftParseJSON MISSING (fmap (`div` 3) . parseJSON) parseJSON

customParse2 :: FromJSON2 f => Value -> Parser (f Int Int)
customParse2 = liftParseJSON2 MISSING (fmap (subtract 10) . parseJSON) parseJSON MISSING (fmap (`div` 3) . parseJSON) parseJSON
