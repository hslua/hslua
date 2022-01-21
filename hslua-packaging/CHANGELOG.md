# Changelog

`hslua-packaging` uses [PVP Versioning][].

## hslua-packaging-2.1.0

Release pending.

-   Added function `documentation`: The documented function
    `documentation` is added and exported from module
    `HsLua.Packaging.Documentation`. It allows to retrieve the
    documentation of a given Lua object.

    This replaces `pushDocumentationFunction`, which has been
    removed from the module.

-   Provide function `opt` to make a parameter optional. The
    function `optionalParameter` is deprecated, use `opt
    (parameter ...)` instead.

-   Added module `Convenience`, which defines many functions to
    make the definition of parameters and results easier for
    the most common types.

-   Update to hslua-objectorientation-2.1.0. Lists are now
    writable. This entails a change to `deftype'`. See the
    changelog of hslua-objectorientation for details.

-   Update to hslua-core 2.1.0 and hslua-marshalling 2.1.0.

## hslua-packaging-2.0.0

Released 2021-10-21.

-   Initially created. Contains modules previously found in the
    modules `Foreign.Lua.Call` and `Foreign.Lua.Module` from
    `hslua-1.3`.

-   Moved module hierarchy from Foreign.Lua to HsLua.

-   Added support for a “since” tag on documented functions;
    allows to mark the library version when a function was
    introduced in its present form.

-   Improved syntax for the creation of documented functions.

-   Documentation for functions is now stored in Lua; a method to
    access it is available as a HaskellFunction.

  [PVP Versioning]: https://pvp.haskell.org
