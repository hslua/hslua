[![Hackage]](https://hackage.haskell.org/package/hslua)

[Hackage]: https://img.shields.io/hackage/v/hslua.svg


Overview
--------

HsLua provides the glue to use Lua with Haskell, and the other way
around. It provides foreign function interace (FFI) bindings,
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
version, currently Lua 5.3.6. Cabal flags make it easy to compile
against a system-wide Lua installation.

### When to use it

You should give HsLua a try if you

- want a ready-made interface to Lua;
- are looking for a way to use pre-existing Lua libraries with your
  Haskell program; or
- need to expose complex Haskell functions to Lua.

HsLua exposes most of Lua's C API via Haskell functions. It offers
improved type-safety when compared to the raw C functions, while
also translating Lua errors to Haskell exceptions. Furthermore,
HsLua provides an convenience functions which make interacting
with Lua straight-forward and safe.

Packages
--------

Requirements differ, so we broke HsLua into multiple packages.
These are the packages that make up HsLua:

  - **lua**: Raw bindings to the Lua interpreter; ships with a
    full Lua implementation, but can be configured to use a
    system-wide installation instead. Serves as the basis for all
    other packages here.

  - **hslua-core**: Wrappers and types that make working with Lua
    less C-like and more idiomatic -- from a Haskell point of
    view.

  - **hslua-marshalling**: Functions and types to marshal and
    unmarshal data between Haskell and Lua.

  - **hslua-packaging**: Package Haskell code into Lua structures;
    exposing functions and constructing modules made easy.

  - **hslua-classes**: Type classes that can make interfacing with
    Lua more convenient.

  - **hslua**: All-in-one collection of the above packages.
