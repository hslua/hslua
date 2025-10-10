# Changelog

`hslua-module-system` uses [PVP Versioning][].

## hslua-module-system-1.2.3

Release pending

-   Allowed time-1.15.

## hslua-module-system-1.2.2

Released 2025-08-09.

-   Fix compilation error.

## hslua-module-system-1.2.2

Released 2025-08-09.

-   Lists of file paths now have a "FilePath list" metatable that
    add list methods.

## hslua-module-system-1.2.1.1

Released 2025-07-23.

-   Fixed the docstring of `exists`.

## hslua-module-system-1.2.1

Released 2025-07-23.

-   Add new function `exists`, which allows to check the existance
    and, optionally, type of a filesystem object at the given
    path.

## hslua-module-system-1.2.0

Released 2025-06-23.

-   Added new functions `read_file` and `write_file`: These are
    convenience functions that makes it easier to work with UTF-8
    encoded filenames. The functions in the Lua standard library
    expect filenames encoded in the system's codepage, often
    leading to subtle bugs.

-   Added new functions `cp`, `rename`, and `rm`, which can be
    used similar to the functions in the `os` standard library,
    but expect paths to be given as UTF-8 instead of a file system
    specific encoding.

-   Added new function `times`: the function allows to obtain the
    modification time and access time of a file or directory.

-   Added new function `xdg`: this function gives easy access to
    XDG directories and search paths.

-   Fixed module export list: the function `cmd` was only added to
    the Lua module, but not exported from the Haskell module.
    Instead, `HsLua.Core.run` was erroneously reexported.

## hslua-module-system-1.1.3

Released 2025-05-21.

-   Improved docs for the `os` field.

-   Added new function `cmd` that runs system commands.

-   Moved `CHANGELOG.md` to the `extra-doc-files` field in the
    cabal file and also added `README.md` to that field.

## hslua-module-system-1.1.2

Released 2024-05-28.

-   Fixed error handling in `with_wd`: exceptions when changing
    directories are now properly converted to Lua errors.

## hslua-module-system-1.1.1

Released 2024-01-18.

-   Relaxed upper bound for text, allowing text-2.1.

## hslua-module-system-1.1.0.1

Released 2023-03-26.

-   Improve doc strings.

## hslua-module-system-1.1.0

-   Update to hslua-2.3; this includes the addition of type
    initializers to the module and type specifiers to the fields.

## hslua-module-system-1.0.3

Released 2023-02-14.

-   Added new function `cputime` and field `cputime_precision`,
    e.g. for benchmarking.

## hslua-module-system-1.0.2

Released 2022-02-19.

-   Adjusted package bounds, for hslua-core and hslua-packaging.

## hslua-module-system-1.0.1

Released 2022-01-29.

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
