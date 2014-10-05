## Changelog

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
