# Changelog

`hslua-module-paths` uses [PVP Versioning][].

## hslua-module-path-1.1.0

Released 2023-03-13.

-   Update to hslua-2.3; this includes the addition of type
    initializers to the module and type specifiers to the fields.

-   Fixed tests for `make_relative` on Windows.

## hslua-module-path-1.0.3

Released 2022-08-19.

-   Fixed `make_relative` for longer base paths: Ensure that the
    function produces correct results in cases where the root
    (base) path has more components than the path that should be
    made relative.

## hslua-module-path-1.0.2

Released 2022-02-19.

-   Adjusted package bounds, for hslua-core, hslua-marshalling,
    and hslua-packaging.

## hslua-module-path-1.0.1

-   Bumped upper bound of hslua-core and hslua-marshalling to
    allow their respective version 2.1.

## hslua-module-path-1.0.0

-   Updated to hslua 2.0.

## hslua-module-path-0.1.0.1

Released 2021-02-06.

-   Changed minimal cabal version to 2.2.

## hslua-module-path-0.1.0

Released 2021-02-02.

-   Fixed `directory`. This was the same as normalize.
-   Improved documentation.
-   Added more tests.

## hslua-module-path-0.0.1

Released 2021-02-01.

-   Initially created.

  [PVP Versioning]: https://pvp.haskell.org
