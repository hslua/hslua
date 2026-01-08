# Changelog

`hslua-module-version` uses [PVP Versioning][].

## hslua-module-version-1.2.0

Released 2026-01-08.

- Require hslua-packaging 2.4 or later.

## hslua-module-version-1.1.1

Released 2024-01-18.

-   Relaxed upper bound for text, and filepath,
    allowing text-2.1, filepath-1.5.

## hslua-module-version-1.1.0

Released 2023-03-13.

-   Update to hslua-2.3; this includes the addition of type
    initializers to the module.

## hslua-module-version-1.0.3

Released 2022-09-01.

-   Allow equality checks with non-version values: A *Version*
    value can now be compared with any value. Previously,
    comparing a version with a value that cannot be interpreted as
    a Version would result in an error, violating the principle of
    least surprise.

## hslua-module-version-1.0.2

Released 2022-02-19.

-   Relax upper bounds, allowing hslua-core-2.2.*,
    hslua-marshalling-2.2.\* and hslua-packaging-2.2.\*.

## hslua-module-version-1.0.1

-   Relaxed upper bound of hslua-core, hslua-marshalling, and
    hslua-packaging, allowing their respective version 2.1.

-   Version objects are now modifiable; setting a list index
    modifies the Version object:

    ``` lua
    local v = Version '5.3.6'
    v[2] = 2
    v[3] = nil
    assert(v == Version '5.2')
    ```

## hslua-module-version-1.0.0

Released 2021-10-22.

  [PVP Versioning]: https://pvp.haskell.org
