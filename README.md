HsLua – Bindings to Lua, an embeddable scripting language
=========================================================

[![Build status][GitHub Actions badge]][GitHub Actions]
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Hackage]](https://hackage.haskell.org/package/hslua)

HsLua provides bindings, wrappers, types, and helper functions to
bridge Haskell and Lua.

[GitHub Actions badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[GitHub Actions]: https://github.com/hslua/hslua/actions
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/main?svg=true
[Hackage]: https://img.shields.io/hackage/v/hslua.svg


Overview
--------

[Lua](https://lua.org) is a small, well-designed, embeddable
scripting language. It has become the de-facto default to make
programs extensible and is widely used everywhere from servers
over games and desktop applications up to security software and
embedded devices. This package provides Haskell bindings to Lua,
enable coders to embed the language into their programs, making
them scriptable.

HsLua ships with batteries included and includes Lua 5.3.6. Cabal
flags make it easy to compile against a system-wide Lua
installation.

Packages
--------

Below are the packages which make up HsLua:

  - **lua**: Raw bindings to the Lua interpreter; ships with a
    full Lua implementation, but can be configured to use a
    system-wide installation instead. Serves as the basis for all
    other packages here.

  - **lua-arbitrary**: Make it easier to check Lua functions by
    making the relevant types instances of QuickCheck's Arbitrary
    typeclass.

  - **hslua-core**: Wrappers and types which make working with Lua
    less C-like and more idiomatic -- from a Haskell point of
    view.

  - **tasty-hslua**: Helper functions for writing tasty tests to
    check Lua operations.

  - **hslua**: Additional helpers and convenience mechanisms.
