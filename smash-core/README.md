# smash: Combinators for Maybe types

[![Build Status](https://travis-ci.com/emilypi/smash.svg?branch=master)](https://travis-ci.com/emilypi/smash)
[![Hackage](https://img.shields.io/hackage/v/smash.svg)](https://hackage.haskell.org/package/smash)

This package consists of 3 interesting datatypes and their respective monad transformers:

 - [Wedge](https://hackage.haskell.org/package/smash/docs/Data-Wedge.html): Isomorphic to `Maybe (Either a b)`. The `Wedge` datatype represents the coproduct in the category Hask\* of pointed Hask types, called a [wedge sum](https://ncatlab.org/nlab/show/wedge+sum). One can derive this type as follows:

    ```haskell
    Either (Maybe a) (Maybe b)
    ~ (1 + a) + (1 + b)
    -- units are the same via pushout
    ~ 1 + a + b
    ~ Maybe (Either a b)
    ~ Wedge a b
    ```

 - [Can](https://hackage.haskell.org/package/smash/docs/Data-Can.html): Isomorphic to `Maybe (These a b)`. The `Can` datatype represents the product in Hask\*. One can derive this as follows:

    ```haskell
    (Maybe a, Maybe a)
    ~ (1 + a) * (1 + b)
    -- products distribute over coproducts
    ~ 1 + b + a + a*b
    -- coproducts are associative
    ~ 1 + (b + a + a*b)
    ~ 1 + These a b
    ~ Maybe (These a b)
    ~ Can a b
    ```

 - [Smash](https://hackage.haskell.org/package/smash/docs/Data-Smash.html): Isomorphic to `Maybe (a,b)`. The `Smash` datatype represents a special type of product, a
[smash product](https://ncatlab.org/nlab/show/smash+product), in the category Hask\*.  The smash product is a symmetric, monoidal tensor in Hask* that is the quotient of `Can` over `Wedge`. It can be derived as follows:

    ```haskell
    Can a b / Wedge a b
    ~ 1 + a + b + a*b / 1 + a + b
    -- reassoc coproduct
    ~ (1 + a + b) + a*b / 1 + a + b
    -- def. of quotient: (1 + a + b) ~ 1
    ~ 1 + a * b
    ~ Maybe (a,b)
    ~ Smash a b
    ```
