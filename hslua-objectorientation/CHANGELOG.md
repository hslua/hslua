# Changelog

`hslua-objectorientation` uses [PVP Versioning][].

## hslua-objectorientation-2.3.0

Release pending.

-   Export new function `initType`: The function ensures that a
    type's metatable is initialized and available from the
    registry.

-   Type info for properties: Properties are amended with
    information on the property's type. The functions `property`,
    `possibleProperty`, and `readonly` each now come with typed
    version `property'`, `possibleProperty'`, and `readonly`'.
    This allows to specify the type of a property value.

-   Functions for object typing info: The functions `udDocs` and
    `udTypeSpec` are added, enabling the generation of typing
    information for UDType objects.

## hslua-objectorientation-2.2.1

Released 2022-06-19.

-   Require hslua-core-2.2.1.

-   Require hslua-marshalling-2.2.1.

## hslua-objectorientation-2.2.0.1

Released 2022-05-20.

-   Relax upper bound for mtl, allow mtl-2.3.

## hslua-objectorientation-2.2.0

Released 2022-02-19.

-   Require version 2.2 of hslua-core and hslua-marshalling.

## hslua-objectorientation-2.1.0

Released 2022-01-29.

-   Allow integers as aliases: Aliases can now be of type
    `AliasIndex`, so integers can now be defined as aliases for
    other properties. The function `alias` now takes an
    `AliasIndex` instead of a `Name`; the change entails
    modifications to the types `UDTypeWithList`, `UDType`, and
    `Member`. Also, `AliasIndex` is made into an instance of the
    Eq and Ord type classes.

-   Reworked list representation of objects, allowing write access
    to list components.

    The `ListSpec` type has been updated and contains now a pair
    of pairs, where the inner pairs define how to push and
    retrieve lists, respectively. Users of the `deftypeGeneric'`
    function will have to update their code.

-   Fixed some integer type declarations in C code. Some variables
    had been given incorrect types, like `int` instead of
    `lua_Integer`. They are usually the same, but may differ in
    some setups.

-   Require hslua-core-2.1.0 and hslua-marshalling-2.1.0, or
    later.

## hslua-objectorientation-2.0.1

Released 2021-11-04.

-   Excludes absent properties from `pairs`: Properties that are
    optional and not present in a sum-type value are no longer
    included in the iterator output produced by `pairs` (i.e., the
    `__pairs` metamethod). Previously, the names of absent
    properties were pushed with a `nil` value.

## hslua-objectorientation-2.0.0

Released 2021-10-21.

-   Published without warning.

  [PVP Versioning]: https://pvp.haskell.org
