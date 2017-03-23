## Changelog

### 0.5.0

* New raw functions for `luaopen_base`, `luaopen_package`, `luaopen_string`,
  `luaopen_table`, `luaopen_math`, `luaopen_io`, `luaopen_os`, `luaopen_debug`
  and their high-level wrappers (with names `openbase`, `opentable` etc.)
  implemented.
* Remove custom versions of `loadfile` and `loadstring`.
* Drop support for GHC versions < 7.8, avoid compiler warnings.
* Ensure no symbols are stripped when linking the bundled lua interpreter.
* Simplify `tostring` function definition. (Sean Proctor)
* Explicitly decprecate `strlen`. (Sean Proctor)
* Add links to lua documentation for functions wrapping the official lua C API.
  (Sean Proctor).

### 0.4.1

* Bugfix(#30): `tolist` wasn't popping elements of the list from stack.

### 0.4.0

* `pushstring` and `tostring` now uses `ByteString` instead of `[Char]`.
* `StackValue [Char]` instance is removed, `StackValue ByteString` is added.
* `StackValue a => StackValue [a]` instance is added. It pushes a Lua array to
  the stack. `pushlist`, `islist` and `tolist` functions are added.
* Type errors in Haskell functions now propagated differently. See the
  `Scripting.Lua` documentation for detailed explanation. This should fix
  segfaults reported several times.
* `lua_error` function is removed, it's never safe to call in Haskell.

Related issues and pull requests: #12, #26, #24, #23, #18.

### 0.3.14

* Pkgconf-based setup removed. Cabal is now using `extra-libraries` to link with Lua.
* `luajit` flag is added to link hslua with LuaJIT.

### 0.3.13

* Small bugfix related with GHCi running under Windows.

### 0.3.12

* `pushrawhsfunction` and `registerrawhsfunction` functions are added.
* `apicheck` flag is added to Cabal package to enable Lua API checking. (useful for debugging)

### 0.3.11

* `luaL_ref` and `luaL_unref` functions are added.
