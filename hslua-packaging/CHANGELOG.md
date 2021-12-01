## Changelog

`hslua-packaging` uses [PVP Versioning](https://pvp.haskell.org).

### hslua-packaging 2.0.0

Release pending.

  - Update to hslua-objectorientation 2.1.0. This entails a change
    to `deftype'`, switching the order of item pusher and
    list-extractor function in the tuple passed as the last
    argument.

  - Update to hslua-core 2.1.0 and hslua-marshalling 2.1.0.

### hslua-packaging 2.0.0

Released 2021-10-21.

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
