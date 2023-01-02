# Changelog

`hslua-module-text` uses [PVP Versioning][].

## hslua-module-text-1.0.3

Release pending.

-   Added new function `toencoding`. This can be used to convert a
    UTF-8 encoded string to a different encoding. Most notably, it
    allows to use the systems default code page (`CP0`) on Windows.

## hslua-module-text-1.0.2

Released 2022-02-19.

-   Adjusted package bounds, for hslua-core, hslua-marshalling,
    and hslua-packaging.

## hslua-module-text-1.0.1

Released 2022-01-29.

-   Relaxed upper bound of hslua-core, hslua-marshalling, and
    hslua-packaging, allowing their respective version 2.1.

## hslua-module-text-1.0.0

Released 2021-10-22.

-   Use hslua 2.0.

## hslua-module-text-0.3.0.1

Released 2020-10-16.

-   Relaxed upper bound for hslua, allow `hslua-1.3.*`.

## hslua-module-text-0.3.0

Released 2020-08-15.

-   Use self-documenting module. This allows to include
    documentation with the module definition, and to auto-generate
    documentation from that. Requires hslua-1.2.0 or newer.

-   Run CI tests with all GHC 8 versions, test stack builds.

## hslua-module-text-0.2.1

Released 2019-05-04.

-   Require at least HsLua v1.0.3: that version has better support
    for modules.

-   Rename `pushModuleText` to `pushModule`. The old name is
    keeped as an alias for now.

## hslua-module-text-0.2.0

Released 2018-09-24.

-   Use hslua 1.0.

## hslua-module-text-0.1.2.2

Released 2018-03-09.

-   Relax upper bound for base.

## hslua-module-text-0.1.2.1

Released 2017-11-24.

-   Add missing test file in the sources archive. This oversight
    had caused some stackage test failures.

## hslua-module-text-0.1.2

Released 2017-11-17.

-   Run tests with Travis CI.
-   Fix problems with GHC 7.8

## hslua-module-text-0.1.1

Released 2017-11-16.

-   Lift restriction on base to allow GHC 7.8.

## hslua-module-text-0.1

Released 2017-11-15.

-   First version. Released on an unsuspecting world.

  [PVP Versioning]: https://pvp.haskell.org
