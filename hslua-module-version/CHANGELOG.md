# Changelog

`hslua-module-version` uses [PVP Versioning][].

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
