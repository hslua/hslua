## Changelog

`hslua-marshalling` uses [PVP Versioning](https://pvp.haskell.org).

### hslua-marshalling 2.0.0

Release 2021-10-21.

- Initially created. Contains modules previously found in the
  modules `Foreign.Lua.Peek` and `Foreign.Lua.Push` from
  `hslua-1.3`.

- Removed most functions from the Userdata module, incl. peekAny,
  pushAny. The functions don't add much value over those in
  `HsLua.Core.Userdata`. Use UDTypes from hslua-packaging for a
  more comfortable method of exposing data via userdata values.
