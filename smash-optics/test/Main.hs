{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) 2020-2026 Emily Pillmore
module Main (main) where

import Data.Bifunctor (bimap)
import Data.Can
import Data.Can.Optics
import Data.Smash
import Data.Smash.Optics
import Data.Wedge
import Data.Wedge.Optics
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "smash-optics"
  [ testGroup "Constructor selection"
    [ testCase "Can constructors select only their exact branch" $ do
        map (\x -> preview _Non x) cs @?= [Just (), Nothing, Nothing, Nothing]
        map (\x -> preview _One x) cs @?= [Nothing, Just 1, Nothing, Nothing]
        map (\x -> preview _Eno x) cs @?= [Nothing, Nothing, Just 2, Nothing]
        map (\x -> preview _Two x) cs @?= [Nothing, Nothing, Nothing, Just (1,2)]
    , testCase "Smash constructors select only their exact branch" $ do
        map (\x -> preview _Nada x) ss @?= [Just (), Nothing]
        map (\x -> preview _Smash x) ss @?= [Nothing, Just (1,2)]
    , testCase "Wedge constructors select only their exact branch" $ do
        map (\x -> preview _Nowhere x) ws @?= [Just (), Nothing, Nothing]
        map (\x -> preview _Here x) ws @?= [Nothing, Just 1, Nothing]
        map (\x -> preview _There x) ws @?= [Nothing, Nothing, Just 2]
    , testCase "pair traversals preserve unmatched branches" $ do
        map (over twoed (\(a,b) -> (a+10,b+20))) cs @?= [Non, One 1, Eno 2, Two 11 22]
        map (over smashed (\(a,b) -> (a+10,b+20))) ss @?= [Nada, Smash 11 22]
    , testCase "type-changing traversals preserve the other payload" $ do
        over oneing show (Two (1 :: Int) True) @?= Two "1" True
        over enoing show (Two True (2 :: Int)) @?= Two True "2"
        over _Here show (Here (1 :: Int) :: Wedge Int Bool) @?= Here "1"
        over _There show (There (2 :: Int) :: Wedge Bool Int) @?= There "2"
    ]
  , testGroup "Traversal properties"
    [ testProperty "Can left traversal agrees with bimap" $ \a b (Fun _ f :: Fun Int Int) ->
        conjoin [over oneing f x === bimap f id x | x <- canValues a b]
    , testProperty "Can right traversal agrees with bimap" $ \a b (Fun _ f :: Fun Int Int) ->
        conjoin [over enoing f x === bimap id f x | x <- canValues a b]
    , testProperty "Can both traversal agrees with bimap" $ \a b (Fun _ f :: Fun Int Int) ->
        conjoin [over twoing f x === bimap f f x | x <- canValues a b]
    , testProperty "Smash traversal agrees with bimap" $ \a b (Fun _ f :: Fun Int Int) ->
        conjoin [over smashing f x === bimap f f x | x <- smashValues a b]
    , testProperty "Wedge traversals agree with bimap" $ \a b (Fun _ f :: Fun Int Int) ->
        conjoin [over here f x === bimap f id x .&&. over there f x === bimap id f x | x <- wedgeValues a b]
    , testProperty "Can traversal identity" $ \a b ->
        conjoin [over twoing id x === x | x <- canValues a b]
    , testProperty "Smash traversal identity" $ \a b ->
        conjoin [over smashing id x === x | x <- smashValues a b]
    , testProperty "Wedge traversal identity" $ \a b ->
        conjoin [over here id x === x .&&. over there id x === x | x <- wedgeValues a b]
    , testProperty "Can traversal composition" $ \a b (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) ->
        conjoin [over twoing f (over twoing g x) === over twoing (f . g) x | x <- canValues a b]
    , testProperty "Smash traversal composition" $ \a b (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) ->
        conjoin [over smashing f (over smashing g x) === over smashing (f . g) x | x <- smashValues a b]
    , testProperty "Wedge traversal composition" $ \a b (Fun _ f :: Fun Int Int) (Fun _ g :: Fun Int Int) ->
        conjoin [over here f (over here g x) === over here (f . g) x .&&.
                 over there f (over there g x) === over there (f . g) x | x <- wedgeValues a b]
    ]
  , testCase "Smash indexed traversal uses left=True and right=False" $
      itoListOf smashing (Smash 1 2 :: Smash Int Int) @?= [(True,1),(False,2)]
  , testCase "Can Each visits both payloads in order" $
      itoListOf each (Two 1 2 :: Can Int Int) @?= [(True,1),(False,2)]
  , testCase "Wedge Each indexes the selected side" $ do
      itoListOf each (Here 1 :: Wedge Int Int) @?= [(True,1)]
      itoListOf each (There 2 :: Wedge Int Int) @?= [(False,2)]
  , testGroup "Isomorphisms and prisms"
    [ testProperty "Can iso agrees with projections and roundtrips" $ \a b ->
        conjoin [view _CanIso x === (canFst x, canSnd x) .&&. review _CanIso (view _CanIso x) === x | x <- canValues a b]
    , testProperty "Smash iso agrees with conversion and roundtrips" $ \a b ->
        conjoin [view _SmashIso x === fromSmash x .&&. review _SmashIso (view _SmashIso x) === x | x <- smashValues a b]
    , testProperty "Wedge iso agrees with conversion and roundtrips" $ \a b ->
        conjoin [view _WedgeIso x === fromWedge x .&&. review _WedgeIso (view _WedgeIso x) === x | x <- wedgeValues a b]
    , testProperty "constructor prism review/preview roundtrips" $ \(a :: Int) (b :: Int) -> conjoin
        [ (preview _One (review _One a :: Can Int Int)) === Just a
        , (preview _Eno (review _Eno b :: Can Int Int)) === Just b
        , (preview _Two (review _Two (a,b) :: Can Int Int)) === Just (a,b)
        , (preview _Smash (review _Smash (a,b) :: Smash Int Int)) === Just (a,b)
        , (preview _Here (review _Here a :: Wedge Int Int)) === Just a
        , (preview _There (review _There b :: Wedge Int Int)) === Just b
        ]
    ]

  ]
  where
    cs = canValues 1 2
    ss = smashValues 1 2
    ws = wedgeValues 1 2

canValues :: Int -> Int -> [Can Int Int]
canValues a b = [Non, One a, Eno b, Two a b]

smashValues :: Int -> Int -> [Smash Int Int]
smashValues a b = [Nada, Smash a b]

wedgeValues :: Int -> Int -> [Wedge Int Int]
wedgeValues a b = [Nowhere, Here a, There b]
