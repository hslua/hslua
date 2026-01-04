# Changelog

`hslua-packaging` uses [PVP Versioning][].

## hslua-packaging-2.4.0

Release pending.

- Modified the *FunctionDoc* type: added the function name to the
  type and changed field names to be more consistent.

- Added function `peekFunctionDoc` to retrieve function
  documentation from the Lua stack.

## hslua-packaging-2.3.2

Released 2025-06-23.

-   Require hslua-objectorientation-2.4.

## hslua-packaging-2.3.1

Released 2024-01-18.

-   Relaxed upper bound for text and containers, allowing
    text-2.1, and containers-0.7.

## hslua-packaging-2.3.0

Released 2023-03-13.

-   Type initializers as part of Module records. This allows to
    associate types with a module. For performance reasons, the
    types are not initialized when the module is pushed, but only
    on first use. However, the documentation Lua object for each
    module now has an additional field `types`. The new field
    contains a function that returns the names of all associated
    types. Calling the function will also initialize these types,
    thereby making the respective metatables available in the
    registry.

-   *Field* records now have an additional `fieldType` entry.
    \[API change\]

-   The `pushUD` function is now specialized to documented types.

-   Export `initType`. The function ensures that the metatable of
    a type has been fully initialized. This can be helpful when
    the default method of lazy initialization is not desired, e.g.
    when the type object is to be inspected or extended.

-   Re-export `udDocs`, `udTypeSpec`, allowing to generate typing
    info for userdata classes.

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
