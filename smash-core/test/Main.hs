{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) 2020-2026 Emily Pillmore
module Main (main) where

import Control.Monad (forM_)
import Control.Monad.State.Strict (modify, runState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Can
import Control.Monad.Trans.Smash
import Control.Monad.Trans.Wedge
import Data.Bifunctor (bimap)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.Can
import Data.Functor.Identity (Identity(..))
import Data.Smash
import Data.Wedge
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Read (readMaybe)

main :: IO ()
main = defaultMain $ testGroup "smash"
  [ representationTests "Can" genCan
  , representationTests "Smash" genSmash
  , representationTests "Wedge" genWedge
  , testGroup "Conversions and instances"
    [ testProperty "Can projections and swap" $ forAll genCan $ \x ->
        (canFst (swapCan x), canSnd (swapCan x)) === (canSnd x, canFst x)
    , testProperty "Smash pointed-product roundtrip" $ forAll genSmash $ \x ->
        toSmash (fromSmash x) === x
    , testProperty "Smash conversion inverse" $ \(x :: Maybe (Int, Int)) ->
        fromSmash (toSmash x) === x
    , testProperty "Wedge pointed-sum roundtrip" $ forAll genWedge $ \x ->
        toWedge (fromWedge x) === x
    , testProperty "Wedge conversion inverse" $ \(x :: Maybe (Either Int Int)) ->
        fromWedge (toWedge x) === x
    , testProperty "Can swap involution" $ forAll genCan $ \x -> swapCan (swapCan x) === x
    , testProperty "Smash swap involution" $ forAll genSmash $ \x -> swapSmash (swapSmash x) === x
    , testProperty "Wedge swap involution" $ forAll genWedge $ \x -> swapWedge (swapWedge x) === x
    , testProperty "Can bimap composition" $ forAll genCan $ \x ->
        bimap (+1) (*2) (bimap (*3) (subtract 4) x) === bimap ((+1) . (*3)) ((*2) . subtract 4) x
    , testProperty "Smash functor composition" $ forAll genSmash $ \x ->
        fmap (+1) (fmap (*2) x) === fmap ((+1) . (*2)) x
    , testProperty "Wedge traversal identity" $ forAll genWedge $ \x ->
        runIdentity (traverse Identity x) === x
    ]
  , testGroup "Binary constructor tags"
    [ testCase "Can tags remain 0, 1, 2, 3" $ forM_
        [(Non, encode (0 :: Int)), (One 7, encode (1 :: Int) <> encode (7 :: Int)),
         (Eno 9, encode (2 :: Int) <> encode (9 :: Int)),
         (Two 7 9, encode (3 :: Int) <> encode (7 :: Int) <> encode (9 :: Int))] $
          \(x :: Can Int Int, bytes) -> encode x @?= bytes
    , testCase "Smash tags remain 0, 1" $ do
        encode (Nada :: Smash Int Int) @?= encode (0 :: Int)
        encode (Smash 7 9 :: Smash Int Int) @?= encode (1 :: Int) <> encode (7 :: Int) <> encode (9 :: Int)
    , testCase "Wedge tags remain 0, 1, 2" $ do
        encode (Nowhere :: Wedge Int Int) @?= encode (0 :: Int)
        encode (Here 7 :: Wedge Int Int) @?= encode (1 :: Int) <> encode (7 :: Int)
        encode (There 9 :: Wedge Int Int) @?= encode (2 :: Int) <> encode (9 :: Int)
    ]
  , transformerTests
  ]

genCan :: Gen (Can Int Int)
genCan = oneof [pure Non, One <$> arbitrary, Eno <$> arbitrary, Two <$> arbitrary <*> arbitrary]

genSmash :: Gen (Smash Int Int)
genSmash = oneof [pure Nada, Smash <$> arbitrary <*> arbitrary]

genWedge :: Gen (Wedge Int Int)
genWedge = oneof [pure Nowhere, Here <$> arbitrary, There <$> arbitrary]

representationTests :: (Eq a, Show a, Read a, Binary a) => String -> Gen a -> TestTree
representationTests name gen = testGroup name
  [ testProperty "Show/Read roundtrip" $ forAll gen $ \x -> readMaybe (show x) === Just x
  , testProperty "Binary roundtrip consumes all bytes" $ forAll gen $ \x ->
      case decodeOrFail (encode x) of
        Left (_, _, err) -> counterexample err False
        Right (rest, _, y) -> (rest, y) === (mempty, x)
  ]

transformerTests :: TestTree
transformerTests = testGroup "Transformers"
  [ testCase "CanT accumulates left values in bind order" $
      runIdentity (runCanT (CanT (Identity (Two [1] (2 :: Int))) >>= \x ->
        CanT (Identity (Two [3] (x + 1))))) @?= Two [1,3 :: Int] 3
  , testCase "SmashT accumulates left values in bind order" $
      runIdentity (runSmashT (SmashT (Identity (Smash [1] (2 :: Int))) >>= \x ->
        SmashT (Identity (Smash [3] (x + 1))))) @?= Smash [1,3 :: Int] 3
  , testCase "CanT empty and left-only cases skip continuation effects" $
      forM_ [Non, One "error"] $ \x ->
        runState (runCanT (CanT (pure x) >>= \(_ :: Int) -> lift (modify (+1)) >> pure 5)) 0
          @?= (x, 0 :: Int)
  , testCase "SmashT empty case skips continuation effects" $
      runState (runSmashT (SmashT (pure Nada) >>= \(_ :: Int) -> lift (modify (+1)) >> pure 5)) 0
        @?= (Nada :: Smash String Int, 0 :: Int)
  , testCase "WedgeT empty and left cases skip continuation effects" $
      forM_ [Nowhere, Here "error"] $ \x ->
        runState (runWedgeT (WedgeT (pure x) >>= \(_ :: Int) -> lift (modify (+1)) >> pure 5)) 0
          @?= (x, 0 :: Int)
  , testProperty "CanT lift preserves state effects" $ \(n :: Int) ->
      runState (runCanT (lift (modify (+1)) >> pure n)) 0 === (Eno n :: Can String Int, 1 :: Int)
  , testProperty "SmashT lift preserves state effects" $ \(n :: Int) ->
      runState (runSmashT (lift (modify (+1)) >> pure n)) 0 === (Smash "" n, 1 :: Int)
  , testProperty "WedgeT lift preserves state effects" $ \(n :: Int) ->
      runState (runWedgeT (lift (modify (+1)) >> pure n)) 0 === (There n :: Wedge String Int, 1 :: Int)
  ]
