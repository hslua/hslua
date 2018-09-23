hslua-aeson
===========

[![travis build status]](https://travis-ci.org/hslua/hslua-aeson)
[![MIT License]](./LICENSE)


Glue to hslua for aeson values.

This provides a `StackValue` instance for aeson's `Value` type. The following
conventions are used:

- `Null` values are encoded as a special value (stored in the registry field
  `HSLUA_AESON_NULL`). Using `nil` would cause problems with null-containing
  arrays.

- Objects are converted to tables in a straight-forward way.

- Arrays are converted to Lua tables. Array-length is included as the value at
  index 0. This makes it possible to distinguish between empty arrays and empty
  objects.

- JSON numbers are converted to Lua numbers (usually doubles), which can cause
  a loss of precision.

License
-------

This project is licensed under the liberal MIT license, the same license under
which hslua and lua itself are published. See the [LICENSE](./LICENSE) file for
details.

[travis build status]: https://img.shields.io/travis/hslua/hslua-aeson/master.svg?style=flat-square
[MIT License]: https://img.shields.io/github/license/hslua/hslua-aeson.svg?style=flat-square
