# Changelog

`hslua-core` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

## hslua-core 2.0.0

Release pending.

- Error handling has been reworked completely. The type of
  exceptions used and handled by HsLua is now exposed to the type
  system. The type `Lua` makes use of a default error type. Custom
  error handling can be implemented by using the `LuaE` type with
  an exception type that is an instance of class `LuaError`.

- Added new module HsLua.Core.Userdata. It contains thin wrappers
  around the functions available for creating
  Haskell-value-wrapping userdata objects.

- Added new module HsLua.Core.Closures, containing functions to
  expose Haskell functions to Lua.

## hslua-core 1.0.0

Released 2021-02-27.

Extracted from hslua-1.3.0.
