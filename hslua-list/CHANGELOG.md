# Changelog

`hslua-list` uses [PVP Versioning](https://pvp.haskell.org).

## hslua-list-1.1.4

Released 2024-10-01.

-   Added function `List.iter`; it returns an iterator function
    that returns the next list item each time it is called.

-   `List.extend` now returns the extended list instead of the
    list that was appended.

## hslua-list-1.1.3

Released 2024-09-26.

-   Fixed a bug that prevented `List.filter` to be used on tables
    that don't have a metatable.

-   Added a stub function to ensure that the module can be
    compiled against Lua 5.3.

-   Lowered the lower bound for hslua-core, allowing hslua-core
    2.1.

## hslua-list-1.1.2

Released 2024-09-20.

-   Lists can now be constructed from iterators.

-   Added method `:at` to access list elements by index; negative
    indices are counted from the end. A second value can be
    passed, which will be returned if there is no item at the
    given index.

## hslua-list-1.1.1

Released 2023-03-17.

-   Conversion to strings: added a `__tostring` that lists all
    elements separated by commas and a space, surrounded by braces
    and prefixed with the metatable's name.

## hslua-list-1.1.0.1

Released 2023-01-23.

-   Ensure that `test/test-list.lua` is included in the release
    tarball.

## hslua-list-1.1.0

Released 2023-03-13.

-   Removed `pushPandocList`. The function was a left-over from
    pandoc-lua-marshal, the place where this package originated.

## hslua-list-1.0.0

Released 2022-10-10.

-   To boldly go where no Haskell library has gone before.
