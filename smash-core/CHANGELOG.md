# Revision history for smash

## 0.1.3 -- 2022-04-03

* Update `base` bounds to allow GHC 9.2.x

## 0.1.2

* Add Monad Transformers for `Can`, `Wedge`, and `Smash` ([#25](https://github.com/emilypi/smash/pull/25))
* Add Safe haskell pragmas
* Add instances for all functor classes.
* Add instances for `MonadZip`
* Add nice pointfree definitions for some functions ([#24](https://github.com/emilypi/smash/pull/24), thanks @subttle!)
* Add unfolds to the Api.
* Add template haskell `Lift` instance ([#20](https://github.com/emilypi/smash/pull/20), thanks @gergoerdi!)
* Fixes for various haddock problems (thank you @lemastero and @L7R7!)
* Bump base to exclude 8.2.x

## 0.1.1

* Add `NFData`, `Binary` instances
* CPP to extend to 8.2.2 without warnings

## 0.1.0.0

* First version. Released on an unsuspecting world.
