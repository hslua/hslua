# Changelog

`hslua-module-system` uses [PVP Versioning][].

## hslua-module-system-1.0.2

Released 2022-02-19.

-   Adjusted package bounds, for hslua-core and hslua-packaging.

## hslua-module-system-1.0.1

Released 29-01-2022.

-   Relaxed upper bound of hslua-core, hslua-marshalling, and
    hslua-packaging, allowing their respective version 2.1.

## hslua-module-system-1.0.0

Released 2021-10-21.

-   Use hslua 2.0.

## hslua-module-system-0.2.2.1

Released 2020-10-16.

-   Relaxed upper bound for hslua, allow `hslua-1.3.*`.

## hslua-module-system-0.2.2

Released 2020-08-15.

-   Relaxed upper bound for hslua, allow `hslua-1.2.*`.
-   Improved documentation of internal types.
-   Use tasty-lua for unit tests.
-   Update CI to test with all GHC versions.

## hslua-module-system-0.2.1

Released 2019-05-04.

-   Use module helpers made available with HsLua 1.0.3. This
    avoids code duplication when used with other hslua modules.

## hslua-module-system-0.2.0

Released 2019-05-01.

All fields and functions are now exported from the Haskell module
under the same name as that used in Lua.

### New fields

-   `arch`: processor architecture.
-   `compiler_name`: Haskell compiler that was used to compile the
    module.
-   `compiler_version`: version of the compiler.
-   `os`: operating system.

### New functions

-   `mkdir`: create a new directory.
-   `rmdir`: remove a directory.
-   `with_env`: perform action with custom environment.
-   `with_wd`: perform action in another directory.

### Removed or renamed functions

-   `currentdir` was renamed to `getwd`.
-   `chdir` was renamed to `setwd`.
-   `pwd` was removed.

### Misc

-   Fix typos and copy-paste errors in docs, tests.

## hslua-module-system-0.1.0

Released 2019-04-26.

-   First version. Released on an unsuspecting world.

  [PVP Versioning]: https://pvp.haskell.org
