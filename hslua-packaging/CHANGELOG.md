# Changelog

`hslua-packaging` uses [PVP Versioning][].

## hslua-packaging-2.3.0

Release pending.

-   *Field* records now have an additional `fieldType` entry.
    \[API change\]

-   (Re)export `initType` from `HsLua.Packaging.UDType`. The
    function is defined in package `hslua-objectorientation` and
    ensures that the metatable of a type has been fully
    initialized. This can be helpful when the default method of
    lazy initialization is not desired, e.g. when the type object
    is to be inspected or extended.

## hslua-packaging-2.2.1

Release 2022-06-19.

-   Require hslua-core-2.2.1.

-   Require hslua-marshalling-2.2.1.

-   Require hslua-objectorientation-2.2.1.

## hslua-packaging-2.2.0.1

Released 2022-05-20.

-   Relax upper bound for mtl, allow mtl-2.3.

## hslua-packaging-2.2.0

Released 2022-02-19.

-   Require versions 2.2 for hslua-core, hslua-marshalling,
    hslua-objectorientation.

## hslua-packaging-2.1.0

Released 2022-01-29.

-   Added function `documentation`: The documented function
    `documentation` is added and exported from module
    `HsLua.Packaging.Documentation`. It allows to retrieve the
    documentation of a given Lua object.

    This replaces `pushDocumentationFunction`, which was removed.

-   Cleanup of Function module:

    -   `docsField` was moved to module Documentation.
    -   `pushDocumentation` is renamed to `getdocumentation` and
        moved to the Documentation module. It now returns the Lua
        type of the retrieved documentation value.

-   Function `registerDocumentation` was changed: the documentation
    is no longer passed in but must be at the top of the stack.

-   New functions `pushModuleDoc`, and `pushFunctionDoc`, pushing
    structured documentation objects for models and functions,
    respectively.

-   Provide function `opt` to make a parameter optional. The
    function `optionalParameter` is deprecated, use `opt
    (parameter ...)` instead.

-   Added function `udresult`; it defines a function result and is
    analogous to the existing `udparam` function.

-   Added module `Convenience`, which defines many functions to
    make the definition of parameters and results easier for
    the most common types.

-   Pushing a documented module now also registers the module's
    documentation.

-   The module HsLua.Packaging.Rendering has been deprecated. It
    is no longer exported as part of HsLua.Packaging and must be
    imported explicitly if needed. It may be removed in the
    future. Use Lua objects retrievable with `getdocumentation`
    together with a custom renderer instead.

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
