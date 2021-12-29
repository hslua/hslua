# Changelog

`hslua-aeson` uses [PVP Versioning][].

## hslua-aeson-2.1.0

Release pending.

-   Update to hslua 2.1.

-   Encode `null` as light userdata: The `NULL` pointer wrapped
    into a light userdata is used to encode the JSON null value.

    The `pushNull` function has been removed; use
    `pushValue Null` instead.

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
