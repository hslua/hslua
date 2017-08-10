## Changelog
### 0.6.0

* Supported Lua Versions now include Lua 5.2 and Lua 5.3. LuaJIT and Lua 5.1
  remain supported as well.
* Flag `use-pkgconfig` was added to allow discovery of library and include paths
  via pkg-config. Setting a specific Lua version flag now implies `system-lua`.
  (Sean Proctor)
* The module was renamed from `Scripting.Lua` to `Foreign.Lua`. The code is now
  split over multiple sub-modules. Files processed with hsc2hs are restricted to
  Foreign.Lua.Api.
* A `Lua` monad (reader monad over LuaState) is introduced. Functions which took
  a LuaState as their first argument are changed into monadic functions within
  that monad.
* Error handling has been redesigned completely. A new LuaException was
  introduced and is thrown in unexpected situations. Errors in lua which are
  leading to a `longjmp` are now caught with the help of additional C wrapper
  functions. Those no longer lead to uncontrolled program termination but are
  converted into a LuaException.
* `peek` no longer returns `Maybe a` but just `a`. A LuaException is thrown if
  an error occurs (i.e. in situtations where Nothing would have been returned
  previously).
* The `StackValue` typeclass has been split into `FromLuaStack` and
  `ToLuaStack`. Instances not satisfying the law `x == push x *> peek (-1)` have
  been dropped.
* Documentation of API functions was improved. Most docstrings have been copied
  from the official Lua manual, enriched with proper markup and links, and
  changed to properly describe hslua specifics when necessary.
* Example programs have been moved to a separate repository.
* Unused files were removed. (Sean Proctor)

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
