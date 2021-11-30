# Changelog

`hslua-objectorientation` uses [PVP Versioning][1].

## hslua-objectorientation 2.1.0

Release pending.

  - Allow integers as aliases: Aliases can now be of type
    `AliasIndex`, so integers can now be defined as aliases for
    other properties. The function `alias` now takes an
    `AliasIndex` instead of a `Name`; the change entails
    modifications to the types `UDTypeWithList`, `UDType`, and
    `Member`. Also, `AliasIndex` is made into an instance of the
    Eq and Ord type classes.

  - Switch types in `ListSpec` pair. This change is made to unify
    the code: Properties already had the pusher as the first item
    in the tuple, which is now matched in this type. Users of the
    `deftype'` function will have to update their code.

  - Fixed some integer type declarations in C code. Some variables
    had been given incorrect types, like `int` instead of
    `lua_Integer`. They are usually the same, but may differ in
    some setups.

## hslua-objectorientation 2.0.1

Release 2021-11-04.

  - Excludes absent properties from `pairs`: Properties that are
    optional and not present in a sum-type value are no longer
    included in the iterator output produced by `pairs` (i.e., the
    `__pairs` metamethod). Previously, the names of absent
    properties were pushed with a `nil` value.

## hslua-objectorientation 2.0.0

Release 2021-10-21.

- Published without warning.

[1]: https://pvp.haskell.org
