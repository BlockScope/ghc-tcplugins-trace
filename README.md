# Plugins for Blobs

[![cabal](https://github.com/BlockScope/plugins-for-blobs/actions/workflows/cabal.yml/badge.svg)](https://github.com/BlockScope/plugins-for-blobs/actions/workflows/cabal.yml)
[![stack](https://github.com/BlockScope/plugins-for-blobs/actions/workflows/stack.yml/badge.svg)](https://github.com/BlockScope/plugins-for-blobs/actions/workflows/stack.yml)

> 1 blob is equal to 1 lbf⋅s2/in, or 12 [slugs][slug].

This is an attempt to refactor the [thoralf-plugin][thoralf-plugin] and the
[uom-plugin][uom-plugin]. Under the hood, the `thoralf-plugin` uses Z3 to solve
equational theories. The `uom-plugin` solves trivial equations, rewrites and
simplifies constraints and makes substitutions.

[slug]: https://en.wikipedia.org/wiki/Slug_(unit)
[uom-plugin]: https://github.com/adamgundry/uom-plugin
[thoralf-plugin]: https://github.com/bgamari/the-thoralf-plugin
[ghc-tcplugins-extra]: https://github.com/BlockScope/ghc-tcplugins-extra
[units-parser]: https://github.com/adamgundry/units-parser

The `uom-plugin` depends on [ghc-tcplugins-extra][ghc-tcplugins-extra] and
[units-parser][units-parser]. It defines a quasiquoter for writing units with
measures, such as `[u| 9.8 m/s^2 |]`.

The `thoralf-plugin` ships with one small units example, calculating distance
in `m` from velocity in `m/s` and time in `s`. The `uom-plugin` has a large
number of unit tests.

## Goals

* Use the unit quasiquoter with the `thoralf-plugin` and get it to pass all of
  the units tests of the `uom-plugin`.
* Identify, refactor and extract commonality in both unit plugins.

## Progress

* Added `ghc-corroborate`, a new package exposing a flattened subset of GHC's
  API needed for typechecking plugins as a single API across multiple GHC
  versions. It uses cabal conditionals and mixins and avoids use of the `CPP`
  language extension and predefined macros for switching between GHC versions.
* Forked `ghc-tcplugins-extra` to use `ghc-corroborate` and to remove its use
  of `CPP`.
* Moved the tracing of the `thoralf-plugin` to `ghc-tcplugins-trace`.
* Moved the quasiquoter of the `uom-plugin` to `uom-th`.
* Moved the units of measure (UoM) theory from the `thoralf-plugin` and much of
  the `uom-plugin` internals to `uom-quantity`.
* Pulled unit definitions out of `uom-plugin` and put these into
  `uom-plugin-defs`.
* Rearranged the modules of each plugin for similarity between both.

## Changes in Use

```diff
-- {-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
++ {-# OPTIONS_GHC -fplugin Plugins.UoM #-}
```

```diff
-- {-# OPTIONS_GHC -fplugin ThoralfPlugin.Plugin #-}
++ {-# OPTIONS_GHC -fplugin Plugins.Thoralf #-}
```

## Building

To format `**/package.dhall` files and generate `**/*.cabal` files:

```
> ./stack-shake-build.sh cabal-files
```

## Testing

* ### With Cabal

```
> cabal test all:tests
```
```
> cabal test uom-plugin:units --test-show-details=always --test-options="--color always"
uom-plugin:units
  Get the underlying value with unQuantity
    unQuantity 1: OK
    (see stack section below for the full output)

All 78 tests passed (0.00s)
```

```
> cabal test thoralf-plugin:units --test-show-details=always --test-options="--color always"
thoralf-plugin:units
  Get the underlying value with unQuantity
    unQuantity 3 m:                OK
    unQuantity 3 s^2:              OK
    unQuantity 3 m s^-1:           OK
    unQuantity 3.0 kg m^2 / m s^2: OK
    unQuantity 1:                  OK
    unQuantity 1 (1/s):            OK
    unQuantity 1 1/s:              OK
    unQuantity 1 s^-1:             OK
    unQuantity 2 1 / kg s:         OK
    unQuantity (1 % 2) kg:         OK

All 10 tests passed (0.00s))
```

* ### With Stack

```
> stack test
uom-quantity       > test (suite: doctest)
uom-quantity       > Examples: 74  Tried: 74  Errors: 0  Failures: 0
uom-quantity       > Test suite doctest passed

uom-quantity       > test (suite: hlint)
uom-quantity       > No hints
uom-quantity       > Test suite hlint passed

uom-plugin         > test (suite: defs)
uom-plugin         > Test suite defs passed

uom-plugin         > test (suite: units)
uom-plugin         > uom-plugin
uom-plugin         >   Get the underlying value with unQuantity
uom-plugin         >     unQuantity 3 m:                             OK
uom-plugin         >     unQuantity 3 s^2:                           OK
uom-plugin         >     unQuantity 3.0 kg m^2 / m s^2:              OK
uom-plugin         >     unQuantity 1:                               OK
uom-plugin         >     unQuantity 1 (1/s):                         OK
uom-plugin         >     unQuantity 1 1/s:                           OK
uom-plugin         >     unQuantity 1 s^-1:                          OK
uom-plugin         >     unQuantity 2 1 / kg s:                      OK
uom-plugin         >     unQuantity (1 % 2) kg:                      OK
uom-plugin         >   Attach units by applying the quasiquoter without a numeric value
uom-plugin         >     m 3:                                        OK
uom-plugin         >     m <$> [3..5]:                               OK
uom-plugin         >     m/s 3:                                      OK
uom-plugin         >     s^2 3:                                      OK
uom-plugin         >     1 $ 3:                                      OK
uom-plugin         >     fmap [u| kg |] read $ "3":                  OK
uom-plugin         >     fmap [u| kg |] read $ "3.0":                OK
uom-plugin         >   Showing constants
uom-plugin         >     show 3m:                                    OK
uom-plugin         >     show 3m/s:                                  OK
uom-plugin         >     show 3.2 s^2:                               OK
uom-plugin         >     show 3.0 kg m^2 / m s^2:                    OK
uom-plugin         >     show 1:                                     OK
uom-plugin         >     show 1 s^-1:                                OK
uom-plugin         >     show 2 1 / kg s:                            OK
uom-plugin         >     show (1 % 2) kg:                            OK
uom-plugin         >   Basic operations
uom-plugin         >     2 + 2:                                      OK
uom-plugin         >     in m/s:                                     OK
uom-plugin         >     mean:                                       OK
uom-plugin         >     tricky generalisation:                      OK
uom-plugin         >     polymorphic zero:                           OK
uom-plugin         >     polymorphic frac zero:                      OK
uom-plugin         >   Literal 1 (*:) Quantity _ u
uom-plugin         >     _ = Double:                                 OK
uom-plugin         >     _ = Int:                                    OK
uom-plugin         >     _ = Integer:                                OK
uom-plugin         >     _ = Rational, 1 *: [u| 1 m |]:              OK
uom-plugin         >     _ = Rational, mk (1 % 1) *: [u| 1 m |]:     OK
uom-plugin         >     _ = Rational, 1 *: [u| 1 % 1 m |]:          OK
uom-plugin         >     _ = Rational, mk (1 % 1) *: [u| 1 % 1 m |]: OK
uom-plugin         >   (1 :: Quantity _ One) (*:) Quantity _ u
uom-plugin         >     _ = Double:                                 OK
uom-plugin         >     _ = Int:                                    OK
uom-plugin         >     _ = Integer:                                OK
uom-plugin         >     _ = Int:                                    OK
uom-plugin         >   errors when a /= b, (1 :: Quantity a One) (*:) Quantity b u
uom-plugin         >     b = Double
uom-plugin         >       a = Int:                                  OK
uom-plugin         >       a = Integer:                              OK
uom-plugin         >       a = Rational:                             OK
uom-plugin         >     b = Int
uom-plugin         >       a = Double:                               OK
uom-plugin         >       a = Integer:                              OK
uom-plugin         >       a = Rational:                             OK
uom-plugin         >     b = Integer
uom-plugin         >       a = Double:                               OK
uom-plugin         >       a = Int:                                  OK
uom-plugin         >       a = Rational:                             OK
uom-plugin         >     b = Rational
uom-plugin         >       a = Double:                               OK
uom-plugin         >       a = Int:                                  OK
uom-plugin         >       a = Integer:                              OK
uom-plugin         >   showQuantity
uom-plugin         >     myMass:                                     OK
uom-plugin         >     gravityOnEarth:                             OK
uom-plugin         >     forceOnGround:                              OK
uom-plugin         >   convert
uom-plugin         >     10m in ft:                                  OK
uom-plugin         >     5 km^2 in m^2:                              OK
uom-plugin         >     ratio:                                      OK
uom-plugin         >     100l in m^3:                                OK
uom-plugin         >     1l/m in m^2:                                OK
uom-plugin         >     1l/m in m^2:                                OK
uom-plugin         >     5l in ft^3:                                 OK
uom-plugin         >     2000000l^2 in ft^3 m^3:                     OK
uom-plugin         >     42 rad/s in s^-1:                           OK
uom-plugin         >     2.4 l/h in m:                               OK
uom-plugin         >     1 m^4 in l m:                               OK
uom-plugin         >   errors
uom-plugin         >     s/m ~ m/s:                                  OK
uom-plugin         >     m + s:                                      OK
uom-plugin         >     a ~ a  =>  a ~ kg:                          OK
uom-plugin         >     a ~ b  =>  a ~ kg:                          OK
uom-plugin         >     a^2 ~ b^3  =>  a ~ s:                       OK
uom-plugin         >   read . show
uom-plugin         >     3 m:                                        OK
uom-plugin         >     1.2 m/s:                                    OK
uom-plugin         >     0:                                          OK
uom-plugin         >   read normalisation
uom-plugin         >     1 m/m:                                      OK
uom-plugin         >     -0.3 m s^-1:                                OK
uom-plugin         >     42 s m s:                                   OK
uom-plugin         >
uom-plugin         > All 78 tests passed (0.00s)
uom-plugin         > Test suite units passed

thoralf-plugin     > test (suite: defs)
thoralf-plugin     > Test suite defs passed

thoralf-plugin     > test (suite: rows)
thoralf-plugin     > [1,2] = 1 :> (2 :> VNil)
thoralf-plugin     > reverse [1,2] = 2 :> (1 :> VNil)
thoralf-plugin     > concat [1,2] [3,4] = 1 :> (2 :> (3 :> (4 :> VNil)))
thoralf-plugin     > snoc 3 [1,2] = 1 :> (2 :> (3 :> VNil))
thoralf-plugin     > stripPrefix [1,2] [1,2,3,4] = Just (3 :> (4 :> VNil))
thoralf-plugin     > stripPrefix [3,4] [1,2,3,4] = Nothing
thoralf-plugin     > stripPrefix [] [1,2,3,4] = Just (1 :> (2 :> (3 :> (4 :> VNil))))
thoralf-plugin     > stripPrefix [1,2] [] = Nothing
thoralf-plugin     > price of [9000,12000] = 21000
thoralf-plugin     > Test suite rows passed

thoralf-plugin     > test (suite: units)
thoralf-plugin     > uom-plugin
thoralf-plugin     >   Get the underlying value with unQuantity
thoralf-plugin     >     unQuantity 1: OK
thoralf-plugin     >
thoralf-plugin     > All 1 tests passed (0.00s)
thoralf-plugin     > Test suite units passed

thoralf-plugin     > test (suite: uom)
thoralf-plugin     > UoM examples
thoralf-plugin     > 3 m/s for 3 s = 9.0 m
thoralf-plugin     > 3 m/s for 2 s = 6.0 m
thoralf-plugin     >
thoralf-plugin     > Main
thoralf-plugin     >   distance: OK
thoralf-plugin     >     +++ OK, passed 100 tests.
thoralf-plugin     >
thoralf-plugin     > All 1 tests passed (0.00s)
thoralf-plugin     > Test suite uom passed

uom-plugin-tutorial> test (suite: doctest)
uom-plugin-tutorial> Examples: 64  Tried: 64  Errors: 0  Failures: 0
uom-plugin-tutorial> Test suite doctest passed
Completed 15 action(s).
```

To see tests of `uom-plugin` with color but no build output to the terminal:

```
> stack test uom-plugin --no-terminal --test-arguments "--color=always"
```