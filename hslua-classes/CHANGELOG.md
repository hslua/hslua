# Changelog

`hslua-classes` uses [PVP Versioning][].


## hslua-classes-2.3.1

Released 2024-01-18.

-   Relaxed upper bound for text, containers, and bytestring,
    allowing text-2.1, containers-0.7, and bytestring-0.12.

## hslua-classes-2.3.0

Released 2023-03-13.

-   Require version 2.3.* of HsLua packages hslua-core and
    hslua-marshalling.

## hslua-classes-2.2.0

Released 2022-02-19.

-   Relaxed upper bounds, allowing hslua-core-2.2.0 and
    hslua-marshalling-2.2.0.

## hslua-classes-2.1.0

Released 2022-01-29.

-   Updated to hslua-core 2.1 and hslua-marshalling 2.1.

-   The Peekable class has been remodeled:

    -   Peekable now contains `safepeek`, which is a `Peeker`
        function for the type.

    -   `peek` is no longer part of Peekable, but a normal
        function defined as `forcePeek . safepeek`.

-   HsLua.Class no longer exports `peekList` and
    `peekKeyValuePairs`. Use the functions from HsLua.Marshalling
    instead.

-   The Exposable class is changed to use the `Peek` monad
    instead of `LuaE`, thereby unifying the way errors are
    reported in HsLua.

-   PeekError has been removed; it is now sufficient for
    exception types used with Peekable, Exposable, and Invokable
    to be instances of LuaError.

-   The Invokable type class now has a single parameter. This
    removes the need for the AllowAmbiguousTypes extension and
    makes using `invoke` much more convenient, as the proper error
    type can now be inferred automatically.

-   Added function `pushAsHaskellFunction` to make it even easier
    to use Haskell functions in Lua.

## hslua-classes-2.0.0

Released 2021-10-21.

-   Initially created. Contains modules previously found in the
    `Foreign.Lua.Types` hierarchy from `hslua-1.3`.

  [PVP Versioning]: https://pvp.haskell.org
