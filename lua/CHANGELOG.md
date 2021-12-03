# Changelog

`lua` uses [PVP Versioning][1].

## lua 2.1.0

Release pending.

-   The functions `lua_rawget` and `lua_rawgeti` now return the type of
    the value that was pushed to the stack.

-   Added bindings to unsafe function `lua_arith`. A new type
    `ArithOPCode` for arithmetic operations is added, as are pattern
    synonyms for the supported operations. These are:

    -   LUA_OPADD
    -   LUA_OPSUB
    -   LUA_OPMUL
    -   LUA_OPDIV
    -   LUA_OPIDIV
    -   LUA_OPMOD
    -   LUA_OPPOW
    -   LUA_OPUNM
    -   LUA_OPBNOT
    -   LUA_OPBAND
    -   LUA_OPBOR
    -   LUA_OPBXOR
    -   LUA_OPSHL
    -   LUA_OPSHR

-   Added an ersatz function `hslua_arith` that wraps `lua_arith`
    and catches errors that occur in metamethods.

-   Made types `OPCode`, `StatusCode`, and `GCCode` instances of
    type class `Show`.

-   Removed `hslua_userdata_gc` from `hslua.h`.

## lua 2.0.2

Released 2021-11-26.

- Make sure lualib.h is available through this package. The header
  file contains info on how and under which name the standard
  library is loaded.

## lua 2.0.1

Released 2021-11-03.

- Added bindings to `lua_rotate` and `lua_version`.

## lua 2.0.0.1

Released 2021-10-30.

- Only install includes when using the Lua code shipped with the package
  (Ellie Hermaszewska). Cabal no longer tries to install the header
  files if a system-wide installation is used.

## lua 2.0.0

Release 2021-10-21.

- Module hierarchy moved from `Foreign.Lua.Raw` to `Lua`.

- Documentation has been improved.

- Added new function `withNewState` to run Lua operations.

- New modules `Lua.Ersatz` containing all bindings to safe
  ersatz functions.

- Higher level and enum types have been removed, only the
  low-level "code" types are kept in this package.

- Constants are now represented as pattern synonyms like `LUA_OK`.

- Provide bindings to more functions:
    + `lua_is...` type-checking functions;
    + `lua_pushstring` to push plain CStrings;
    + auxiliary functions
        * `luaL_loadfile`, and
        * `luaL_loadfilex`;
    + unsafe functions
        * `lua_gettable`,
        * `lua_settable`,
        * `lua_getglobal`, and
        * `lua_setglobal`.

- The function `lua_pop` now expects a `CInt` instead of a
  `StackIndex`.

- New StackIndex constructor functions `nthTop`, `nthBottom`,
  `nth`, and `top`.

- Avoid unnecessary modification of HSFUN metatable.

- Various cleanups and test improvements.

## lua 1.0.0

Released 2021-02-18.

- Initially created. Contains all modules in the `Foreign.Lua.Raw`
  hierarchy from `hslua-1.3`. Documentation has been improved.

[1]: https://pvp.haskell.org
