hslua-aeson
===========

[![MIT License]](./LICENSE)

Pushes and retrieves aeson `Value`s to and from the Lua stack.

- `Null` values are encoded as light userdata containing the
  @NULL@ pointer.

- Objects are converted to string-indexed tables.

- Arrays are converted to sequence tables and are given a
  metatable. This makes it possible to distinguish between empty
  arrays and empty objects. The metatable is stored in the
  registry under key `HsLua JSON array`.

- JSON numbers are converted to Lua numbers, i.e., `Lua.Number`;
  the exact C type may vary, depending on compile-time Lua
  configuration.

License
-------

This project is licensed under the MIT license, the same license
under which hslua and lua itself are published. See the
[LICENSE](./LICENSE) file for details.

[MIT License]: https://img.shields.io/github/license/hslua/hslua-aeson.svg?style=flat-square
