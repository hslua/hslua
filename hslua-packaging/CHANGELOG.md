## Changelog

`hslua-packaging` uses [PVP Versioning](https://pvp.haskell.org).

### hslua-packaging 2.0.0

Release 2021-10-21.

- Initially created. Contains modules previously found in the
  modules `Foreign.Lua.Call` and `Foreign.Lua.Module` from
  `hslua-1.3`.

- Moved module hierarchy from Foreign.Lua to HsLua.

- Added support for a "since" tag on documented functions; allows
  to mark the library version when a function was introduced in
  its present form.

- Improved syntax for the creation of documented functions.

- Documentation for functions is now stored in Lua; a method to
  access it is available as a HaskellFunction.
