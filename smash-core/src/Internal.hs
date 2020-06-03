module Internal
( unzipFirst
, undecideFirst
) where

import Data.Bifunctor

unzipFirst :: Bifunctor f => f (a, b) c -> (f a c, f b c)
unzipFirst fabc = (first fst fabc, first snd fabc)

undecideFirst :: Bifunctor f => Either (f a c) (f b c) -> f (Either a b) c
undecideFirst (Left fac) = first Left fac
undecideFirst (Right fbc) = first Right fbc
