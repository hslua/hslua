# Changelog

`hslua-module-zips` uses [PVP Versioning][].

## hslua-module-zip-1.1.5

Release pending

-   Allowed time-1.15.

## hslua-module-zip-1.1.4

Released 2025-06-23.

-   Allowed time-1.14.

## hslua-module-zip-1.1.3

Released 2024-05-05.

-   Fix build on Windows. There are no symlinks on Windows;
    functions dealing with symlinks are missing from zip-archive
    and need a placeholder function.

## hslua-module-zip-1.1.2

Released 2024-05-05.

-   Added a `symlink` method to Entry objects. This allows to
    check whether an entry represents a symbolic link, and where
    it links.

## hslua-module-zip-1.1.1

Released 2024-01-18.

-   Relaxed upper bound for text, and filepath,
    allowing text-2.1, filepath-1.5.

## hslua-module-zip-1.0.0

Released 2023-03-13.

-   Initially created.

[PVP Versioning]: https://pvp.haskell.org
