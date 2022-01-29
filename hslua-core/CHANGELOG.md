# Changelog

`hslua-core` uses [PVP Versioning][].

## hslua-core-2.1.0

Released 29-01-2022.

-   The functions `rawget`, `rawgeti`, and `getref` now return the
    type of the value that was pushed to the stack.

-   A new function `checkstack'` is added to HsLua.Core.Auxiliary
    and exported from the main HsLua.Core module. The function
    throws an exception if the stack cannot be grown to
    accommodate a given number of elements; it is similar to
    `luaL_checkstack`.

-   Added function `requiref`, which safely wraps the unsafe
    `luaL_requiref` function.

-   New functions `pcallTrace`, `callTrace`, `dostringTrace`, and
    `dofileTrace`: behaves like the respective unsuffixed
    functions, but use a message handler that creates a stack
    traceback on error.

-   Added function `rotate`, wrapping `lua_rotate`.

-   Package helper `requirehs` signature changed to

    ``` haskell
    requirehs :: LuaError e
              => Name                 -- ^ modname
              -> (Name -> LuaE e ())  -- ^ openf
              -> LuaE e ()
    ```

    The function creating and pushing the module value now takes
    the module name as an argument. It also behaves more like
    `luaL_requiref` in that it reloads the module if the value in
    the LOADED table is falsy.

## hslua-core-2.0.0.2

Released 2021-11-03.

-   Fixed output of `pushTypeMismatchError` when there is no value
    at the given index. Previously the function would report the
    value as type `string` and now reports it as `no value`.

## hslua-core-2.0.0.1

Released 2021-10-29.

-   Fixed bug in pushTypeMismatchError. The function did not use
    an absolute stack index in one place, which sometimes lead to
    incorrect actual types being reported.

## hslua-core-2.0.0

Released 2021-10-21.

-   Error handling has been reworked completely. The type of
    exceptions used and handled by HsLua is now exposed to the
    type system. The type `Lua` makes use of a default error type.
    Custom error handling can be implemented by using the `LuaE`
    type with an exception type that is an instance of class
    `LuaError`.

-   Added new module HsLua.Core.Userdata. It contains thin
    wrappers around the functions available for creating
    Haskell-value-wrapping userdata objects.

-   Added new module HsLua.Core.Closures, containing functions to
    expose Haskell functions to Lua.

-   Reverted to using the auxlib `luaL_loadfile` function to load
    a Lua file. Previously files were opened and read in Haskell,
    but some functionality of the auxlib function was missing.

## hslua-core-1.0.0

Released 2021-02-27.

Extracted from hslua-1.3.0.

  [PVP Versioning]: https://pvp.haskell.org
