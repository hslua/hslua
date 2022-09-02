# Changelog

`hslua-module-zips` uses [PVP Versioning][].

## hslua-module-zip-1.0.3

Released 2022-08-19.

-   Fixed `make_relative` for longer base zips: Ensure that the
    function produces correct results in cases where the root
    (base) zip has more components than the zip that should be
    made relative.

## hslua-module-zip-1.0.2

Released 2022-02-19.

-   Adjusted package bounds, for hslua-core, hslua-marshalling,
    and hslua-packaging.

## hslua-module-zip-1.0.1

-   Bumped upper bound of hslua-core and hslua-marshalling to
    allow their respective version 2.1.

## hslua-module-zip-1.0.0

-   Updated to hslua 2.0.

## hslua-module-zip-0.1.0.1

Released 2021-02-06.

-   Changed minimal cabal version to 2.2.

## hslua-module-zip-0.1.0

Released 2021-02-02.

-   Fixed `directory`. This was the same as normalize.
-   Improved documentation.
-   Added more tests.

## hslua-module-zip-0.0.1

Released 2021-02-01.

-   Initially created.

  [PVP Versioning]: https://pvp.haskell.org
