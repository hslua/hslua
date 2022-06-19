# Changelog

`hslua-core` uses [PVP Versioning][].

## hslua-core-2.2.1

Released 2022-06-19.

-   Ensure that loadfile works with umlauts in filepath: The OS
    does not necessarily expect filenames to be UTF-8 encoded,
    especially Windows. On non-Windows systems, the current file
    system encoding is now used to convert filenames to C
    strings. On Windows, the `CP_ACP` codepage is used, as
    required by the Windows API.

-   GC managed Lua state: Add new type `GCManagedState` and
    functions `newGCManagedState`, `closeGCManagedState`, and
    `withGCManagedState`. These allow to create and use a Lua
    state in flexible ways in that it does not require the state
    to be closed explicitly. The state will be closed when the
    respective variable is collected.

-   Require lua-2.2.1.

-   Relax upper bound for mtl, allow mtl-2.3.

## hslua-core-2.2.0

Released 2022-02-19.

-    Use lua-2.2.0, which requires Lua 5.4.

-    Rename `newuserdata` to `newuserdatauv` and let it take the
     number of associated uservalues as an additional argument.

     Similarly, `newhsuserdata` is now `newhsuserdatauv`.

-    Rename `getuservalue` and `setuservalue` to `getiuservalue`
     and `setiuservalue`, respectively. Like both functions now
     take an extra argument specifying the number of the uservalue
     that should be retrieved or set.

     It is now possible for `setiuservalue` to fail, so it returns
     a boolean to indicate whether the action was successful.

-    The `GCControl` type has been updated to match the new gc
     control:

     -   The GCStep constructor takes an argument "stepsize";
     -   constructors GCGen and GCInc have been added;
     -   constructors GCSetPause and GCSetStepMul have been removed.

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
