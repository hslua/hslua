hslua-aeson
===========

[![MIT License]](./LICENSE)

Pushes and retrieves aeson `Value`s to and from the Lua stack.

- `Null` values are encoded as a special value (stored in the
  registry field `HSLUA_AESON_NULL`).

- Objects are converted to string-indexed tables.

- Arrays are converted to sequence tables. Array-length is
  included as the value at index 0. This makes it possible to
  distinguish between empty arrays and empty objects.

- JSON numbers are converted to Lua numbers (usually doubles).

License
-------

This project is licensed under the MIT license, the same license
under which hslua and lua itself are published. See the
[LICENSE](./LICENSE) file for details.

[MIT License]: https://img.shields.io/github/license/hslua/hslua-aeson.svg?style=flat-square
