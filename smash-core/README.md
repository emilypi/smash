# smash: Combinators for Maybe types

[![Hackage](https://img.shields.io/hackage/v/smash.svg)](https://hackage.haskell.org/package/smash)

This package consists of 3 datatypes: [Wedge](https://hackage.haskell.org/package/smash/docs/Data-Wedge.html), [Can](https://hackage.haskell.org/package/smash/docs/Data-Can.html), and [Smash](https://hackage.haskell.org/package/smash/docs/Data-Smash.html).

You can imagine these three types as `Maybe (Either a b)`, `Maybe (Either a (Either b (a,b))`, and `Maybe (These a b)` respectively. It turns out that that each of these datatypes has spcial properties:

- the `Wedge` datatype represents the coproduct (like, `Either`) in the category Hask* of pointed Hask types, called a <https://ncatlab.org/nlab/show/wedge+sum wedge sum>. One can derive this by noting that units are the same in Haskell, and the sum of two pointed types is `(1 + a) + (1 + b) ~ 1 + a + b ~ Wedge a b`.

- the `Can` datatype represents the product (like, `(,)`) in Hask*. You can derive this by considering the product of two pointed types `(1 + a) * (1 + b) ~ 1 + a + b + a*b ~ Can a b`.

- the `Smash` datatype represents a special type of product, a
<https://ncatlab.org/nlab/show/smash+product smash product>, in the category Hask\*.  The smash product is a symmetric, monoidal tensor in Hask* that plays nicely with the product, 'Can', and coproduct, 'Wedge'.


Pictorially, these datatypes look like this:

```
'Can':
        a
        |
Non +---+---+ (a,b)
        |
        b

'Wedge':
                a
                |
Nowhere +-------+
                |
                b


'Smash':


Nada +--------+ (a,b)
```
