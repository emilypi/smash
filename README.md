# Smash


This repository is an ongoing work to mine a particularly interesting area of previously unmined Haskell material by putting geometry and topology to work in Haskell's type system. The libraries listed in this repository are part of an initial effort from Emily's Hulk Smash talks from Haskell Love 2020: 

- [Slides](https://github.com/cohomolo-gy/Hulk-Smash/blob/master/Smash.pdf)
- [Video](https://www.youtube.com/watch?v=4aQlFMvKgdc)

## Projects

This repository is the toplevel for the following projects: 

- [smash](https://hackage.haskell.org/package/smash)
- [smash-aeson](https://hackage.haskell.org/package/smash-aeson)
- [smash-microlens](https://hackage.haskell.org/package/smash-microlens)
- [smash-lens](https://hackage.haskell.org/package/smash-lens)
- [smash-optics](https://hackage.haskell.org/package/smash-optics)


## Development

The supported compiler matrix is GHC 9.6–9.14. Each package has a Tasty suite
combining HUnit regression cases and QuickCheck properties.

```sh
cabal build all --enable-tests
cabal test all --test-show-details=direct
cabal haddock all
cabal sdist all
```

CI also builds and tests the source distributions and checks the JSON instances
against `aeson` 2.0, 2.1, 2.2, and 2.3 to preserve the documented wire formats.
