---
layout: default
author: Albert Krewinkel
title: HsLua
date: 2021-06-16
---

[![Hackage]](https://hackage.haskell.org/package/hslua)

HsLua provides bindings, wrappers, types, and helper functions to
bridge Haskell and Lua.

[Hackage]: https://img.shields.io/hackage/v/hslua.svg


Overview
--------

HsLua provides the glue to use Lua with Haskell, and the other way
around. It provides foreign function interface (FFI) bindings,
helper functions, and as well as many utilities.

[Lua](https://lua.org) is a small, well-designed, embeddable
scripting language. It has become the de-facto default when making
programs extensible, and it is widely used everywhere from servers
over games and desktop applications up to security software and
embedded devices. This package provides Haskell bindings to Lua,
enabling Haskell developers to embed the language into their
programs, to make them scriptable, and to expose relevant Haskell
code to Lua.

HsLua ships with batteries included and includes a recent Lua
version, currently Lua 5.4.8. Cabal flags make it easy to compile
against a system-wide Lua installation.

### Use-cases

You should give HsLua a try if you

- want a ready-made interface to Lua;
- are looking for a way to use pre-existing Lua libraries with your
  Haskell program; or
- need to expose complex Haskell functions to Lua.

HsLua exposes most of Lua's C API via Haskell functions. It offers
improved type-safety when compared to the raw C functions, while
also translating Lua errors to Haskell exceptions. Furthermore,
HsLua provides convenience functions and helpers that make
interacting with Lua straight-forward and safe.

### Showcases

Possibly the best-known real world use case of HsLua is [pandoc],
the universal document converter, where it serves as a central
building block for [Lua filters] and [custom writers].

[Santa's little Lua scripts], originally written for [Advent of
Haskell], is a friendly introduction that showcases how an Haskell
application can be extended through Lua.

[pandoc]: https://pandoc.org
[Lua filters]: https://pandoc.org/lua-filters.html
[custom writers]: https://pandoc.org/MANUAL.html#custom-writers
[Santa's little Lua scripts]: ./santas-little-lua-scripts.html
[Advent of Haskell]: https://adventofhaskell.com/

Example
-------

Expose a Haskell function to Lua and call it from Lua.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
import Control.Monad (void)
import Data.Version (makeVersion)
import HsLua
import Prelude

-- | Factorial function.
factorial :: DocumentedFunction e
factorial = defun "factorial"
  ### liftPure (\n -> product [1..n] :: Prelude.Integer)
  --                 get arg      type of arg      name  description
  <#> parameter      peekIntegral "integer"        "n"   "input number"
  =#> functionResult pushIntegral "integer|string"       "factorial of n"
  #? "Computes the factorial of an integer."
  `since` makeVersion [1,0,0]

main :: IO ()
main = run @HsLua.Exception $ do
  openlibs
  pushDocumentedFunction factorial *> setglobal "factorial"
  -- run a script
  void . dostring $ mconcat
    [ "print(' 5! =', factorial(5), type(factorial(5)))\n"
    , "print('30! =', factorial(30), type(factorial(30)))\n"
    ]
```

Running this program yields

     5! =   120     number
    30! =   265252859812191058636308480000000       string

Note that the second result is too large for a Lua 64 bit integer,
so the value is represented as a string.

### Generated documentation

The documentation can be rendered as pandoc Markdown:

```
### factorial (n)

Calculates the factorial of a positive integer.

*Since: 1.0.0*

Parameters:

n
:   number for which the factorial is computed (integer)

Returns:

 - product of all integers from 1 upto n (integer)
```


Packages
--------

Requirements differ, HsLua is divided into multiple packages.
Three types of packages are bundled in this repository. Base
packages, packages for testing, and module packages.

Base packages offer an increasing level of abstraction, from raw
bindings to the C API up to self-documenting, object-oriented
functions, types, and modules. Testing packages provide helpers to
test Haskell and Lua code, and the module packages each contain a
ready-made module to be used in an application.

### Base packages

Below are the base packages that provide the main functionality of
HsLua. Each module depends on the ones above it.

  - **lua**: Raw bindings to the Lua interpreter; ships with a
    full Lua implementation, but can be configured to use a
    system-wide installation instead. Serves as the basis for all
    other packages here.

  - **[hslua-core][]**: Wrappers and types that make working with
    Lua less C-like and more idiomatic -- from a Haskell point of
    view.

  - **hslua-marshalling**: Functions and types to marshal and
    unmarshal basic Haskell values from and to Lua.

  - **hslua-objectorientation**: Push Haskell values as
    object-like Lua userdata with a high level of abstraction.

  - **hslua-packaging**: Framework to create self-documenting Lua
    functions and modules; package Haskell code and data into Lua
    structures.

  - **hslua-classes**: Type classes that can make interfacing with
    Lua more convenient.

  - **hslua**: Bundle of all base packages, re-exporting all of
    the most important modules.

[hslua-core]: https://hslua.org/hslua-core

### Testing packages

  - **lua-arbitrary**: Make it easier to check Lua functions by
    making the relevant types instances of QuickCheck's Arbitrary
    typeclass.

  - **tasty-hslua**: Helper functions for writing tasty tests to
    check Lua operations.

  - **[tasty-lua][]**: Build test suites for Lua modules; provides
    a very basic Lua-testing framework that can be run and
    presented with other tasty tests.

[tasty-lua]: https://hslua.org/tasty-lua

### Module packages

These packages each contain a documented module that can be
registered in Lua.

  - **hslua-module-path**: Functions and helpers to work with file
    paths in a platform independent manner.

  - **hslua-module-system**: Module wrapper around Haskell's
    System module; provides access to system information and file
    system operations.

  - **hslua-module-text**: a limited, but UTF-8 aware subset of
    Lua's `string` module.

  - **hslua-module-version**: module to handle software versions;
    wrapper around the `Data.Version.Version` data type.

Talks
-----

HsLua was the topic of a talk presented at BOBKonf 2022
([Slides](bobconf-2022.html)).
