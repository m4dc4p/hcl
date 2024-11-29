# HCL Change Log

## v.1.9
* Updated to a more recent Haskell Stack snapshot
* Implemented `MonadThrow` and `MonadCatch` on `Request`

## v.1.8

* updated to haskell stack
* implemented MonadFail
* better documentation

## v1.7.1

* added `ChangeLog.md` to cabal file
* request functions with fallbacks catch `IOError` and fallback appropriately
* code refactoring

## v1.7

* `reqIO` now catches `IOError` and returns `Nothing`
* implemented `reqLiftMaybe`

## v1.6

* added test suite
* fixed compiler warnings
* documemtation fixes
* defined `Request` as `Alternative` and `MonadPlus`

## v1.5.1

* fixed broken cabal file

## v1.5

* modified code to compile against QuickCheck 2.*
  * made `Request` a `Functor` and `Applicative`
* implemented `reqChar` and `reqPassword`

## v1.4

* QuickCheck 2 updates
  * Thanks to Sergei Trofimovich for a patch which makes sure HCL compiles against QuickCheck 1.
