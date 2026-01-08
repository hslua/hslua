# Changelog

`hslua-cli` uses [PVP Versioning](https://pvp.haskell.org).

## hslua-cli-1.4.4

Released 2026-01-08.

-   Dropped support for GHC older than 9.6.

## hslua-cli-1.4.3

Released 2024-06-28.

-   Include copyright notice when printing version info; i.e., the
    behavior is now closer to that of the default `lua`
    interpreter.

-   Avoid "redundant import" warning when compiling with GHC 9.10.

## hslua-cli-1.4.2

Released 2024-01-18.

-   Relaxed upper bound for text, allowing text-2.1.

## hslua-cli-1.4.1

Released 2023-03-18.

-   Always start the REPL if the `-i` parameter is given on the
    command line. This fixes a bug where the REPL would not start
    if `-v`, `-e` or `-l` where given.

## hslua-cli-1.4.0.1

Released 2023-03-17.

-   Fix building on Windows.

## hslua-cli-1.4.0

Released 2023-03-16.

-   Isocline-based REPL: interactive mode is now supported with
    the help of a new repl built with the isocline library.

## hslua-cli-1.3.0

Released 2023-03-13.

-   Require hslua-core 2.3.

## hslua-cli-1.2.0

Released 2022-09-27.

-   The function `runStandalone` now takes two additional
    arguments, the program name and list command line args.

## hslua-cli-1.1.0

Released 2022-09-26.

-   Added support for the `LUA_INIT` environment variable. The
    behavior is as described in the Lua reference manual.

-   Warnings are now enabled when flag `-W` is given.

## hslua-cli-1.0.0

Released 2022-09-23.

-   Initial release.
