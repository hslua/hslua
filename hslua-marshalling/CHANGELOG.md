# Changelog

`hslua-marshalling` uses [PVP Versioning][].

## hslua-marshalling-2.1.0

Release pending.

-   Updated to hslua-core-2.1.0.

-   The `Success` constructor of the `Result` type is now strict;
    the `Failure` constructor remains lazy.

-   The stack is checked before pushing or retrieving nested
    structures: Pushing or peeking a deeply nested structure could
    lead an overflow of the Lua stack. The functions `pushList`,
    `pushSet`, and `pushKeyValuePairs`, as well as `peekList`,
    `peekSet`, and `peekKeyValuePairs` now check that sufficient
    stack space is available before pushing another value to the
    stack.

-   The function `toByteString` now requires a slot on the stack
    if the value at the given index is a number. It checks for
    available space before pushing to the stack, returning
    `Nothing` if no space is left on the stack.

-   The `withContext` function is made more useful and now
    differs from `retrieving`. The string “retrieving” is added
    to the error context by `retrieving`, so `withContext` allows
    to define contexts without this prefix.

-   New convenience function `pushAsTable`, making it easier to
    define a pusher function for values marshaled as tables.

## hslua-marshalling-2.0.1

Released 2021-11-04.

-   Allow `pushIterator` to skip values: If the function that
    pushes the values of a list item signals that it didn’t push
    any values, then that value will be skipped.

## hslua-marshalling-2.0.0

Released 2021-10-21.

-   Initially created. Contains modules previously found in the
    modules `Foreign.Lua.Peek` and `Foreign.Lua.Push` from
    `hslua-1.3`.

-   Removed most functions from the Userdata module,
    incl. peekAny, pushAny. The functions don’t add much value
    over those in `HsLua.Core.Userdata`. Use UDTypes from
    hslua-packaging for a more comfortable method of exposing data
    via userdata values.

  [PVP Versioning]: https://pvp.haskell.org
