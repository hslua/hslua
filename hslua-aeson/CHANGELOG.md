# Changelog

`hslua-aeson` uses [PVP Versioning][].

## hslua-aeson-2.3.2

Released 2026-01-08.

-   Dropped support for aeson < 2.0.3.

## hslua-aeson-2.3.1.1

Released 2024-07-09.

-   Relaxed upper bound for hashable, allowing hashable-1.5.

## hslua-aeson-2.3.1

Released 2024-01-18.

-   Relaxed upper bound for aeson. This required changes to the
    testsuite: The arbitrary JSON values produced by current aeson
    versions include numbers that cannot be converted to Lua
    numbers without loss of precision. Those are first converted
    to representable numbers before round-tripping is tested.

-   Relaxed upper bound for text, containers, and bytestring,
    allowing text-2.1, containers-0.7, and bytestring-0.12.

## hslua-aeson-2.3.0.1

Released 2023-03-13.

-   Relax upper bound for hslua-marshalling, allow version 2.3.

## hslua-aeson-2.3.0

Released 2023-02-21.

-   The `peekValue` peeker now checks for a `__toaeson` metafield
    or `__tojson` metamethod and uses them to compute the `Value`
    of an object:

    The `__toaeson` metafield, if set, must be a function pushed
    via `pushToAeson`. That function is called on a given object,
    and the returned *Value* becomes the result of calling
    `peekValue`.

    Likewise, the `__tojson` metamethod must be a function that
    returns a valid JSON string. The result in that case is the
    decoded string.

    If both, `__toaeson` and `__tojson` are set, then `__toaeson`
    takes precedent.

-   The test suite now has *tasty-hunit* as an additional
    dependency.

## hslua-aeson-2.2.1

Released 2022-06-23.

-   Export `jsonarray`, which is the name of the registry slot
    holding the metatable given to array tables. Setting the
    corresponding registry value will affect all newly created
    array values.

## hslua-aeson-2.2.0.1

Released 2022-06-16.

-   Relaxed upper bound for mtl, allowing mtl-2.3.

-   Relaxed upper bound for aeson, allowing aeson-2.1.

## hslua-aeson-2.2.0

Released 2022-02-19.

-   Relaxed upper bound for hslua-core and hslua-marshalling,
    allowing version 2.2 of both packages.

## hslua-aeson-2.1.0

Released 2022-01-29.

-   Update to hslua 2.1.

-   Encode `null` as light userdata: The `NULL` pointer wrapped
    into a light userdata is used to encode the JSON null value.

    The `pushNull` function has been removed; use
    `pushValue Null` instead.

-   Types that are instances of `ToJSON` and `FromJSON` can be
    marshalled/unmarshalled by using the new functions
    `pushViaJSON` and `peekViaJSON`, respectively.

-   The functions `peekVector`, `pushVector`, `peekScientific`,
    `pushScientific`, `peekKeyMap`, and `pushKeyMap` are
    considered an implementation detail and are no longer
    exported.

-   Array elements are now marked with a metatable. This avoids
    the need for an extra `0` element in the table and offers
    flexibility for users who want to give special behavior to
    lists. The newly exported value `jsonarray` contains the name
    of the registry slot under which the metatable is stored. The
    table can be modified or replaced as required.

## hslua-aeson-2.0.1

Released 2021-12-28.

-   Restored compatibility with aeson 1.5.

## hslua-aeson-2.0.0

Released 2021-12-17.

-   Changed module name from `Foreign.Lua.Aeson` to
    `HsLua.Aeson`.

-   The Peekable and Pushable instances have been removed. The
    package no longer defines orphaned instances.

-   Updated hslua and aeson to the respective 2.0 version.

## hslua-aeson-1.0.3.1

Released 2020-10-16.

-   Allow hslua-1.3.\*.

## hslua-aeson-1.0.3

Released 2020-08-15.

-   Relaxed version constraint for hslua, allowing `hslua-1.2.*`.

## hslua-aeson-1.0.2

Released 2020-05-28

-   Relaxed version constraint for aeson, allowing `aeson-1.5.*`.

-   Update CI tests to check with GHC versions 8.0 through 8.10.
    Compilation with GHC 7.10 is no longer tested.

-   Bump to stackage LTS-14.

## hslua-aeson-1.0.1

Released 2020-04-03

-   Relax version constraint for packages hashable and hslua,
    allow `hashable-1.3` and `hslua-1.1.*`.

## hslua-aeson-1.0.0

Released 2019-09-24.

-   Update to hslua 1.0.0

-   Function `registerNull` has been replaced by `pushNull`.

    Using `pushNull` has the advantage that users won’t have to
    remember to register a special variable. Users who need a
    global variable can set it by running

          pushNull
          setglobal "HSLUA_AESON_NULL"

## hslua-aeson-0.3.0

Released 2017-08-18.

-   Update to hslua 0.8.0.

## hslua-aeson-0.2.0

Not publicly released.

-   Update to hslua 0.6.0.

## hslua-aeson-0.1.0.4

Released 2017-04-17.

-   Ensure compatibility with hslua 0.5.0.

## hslua-aeson-0.1.0.0

Released 2017-02-03.

-   Initial release.

  [PVP Versioning]: https://pvp.haskell.org
