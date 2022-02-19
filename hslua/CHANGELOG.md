# Changelog

`hslua` uses [PVP Versioning][].

## hslua-2.2.0

Released 2022-02-19.

-   Require version 2.2.* of all hslua-* packages.

## hslua-2.1.0

Released 2022-01-29.

-   Update to hslua-objectorientation 2.1.0. This entails changes
    to `deftype'` and `deftypeGeneric`, switching the order of
    item pusher and list-extractor function in the tuple passed as
    the last argument.

-   Update to hslua-core 2.1.0, hslua-marshalling 2.1.0, and
    hslua-classes 2.1.0.

## hslua-2.0.1

Released 2021-11-04.

-   Updated lower bounds of hslua packages:
    -   hslua \>= 2.0.0.2,
    -   hslua-marshalling \>= 2.0.1, and
    -   hslua-objectorientation \>= 2.0.1.

    This fixes a number of smaller issues; see the respective
    package changelogs for details.

## hslua-2.0.0

Released 2021-10-21.

-   Move module hierarchy from Foreign.Lua to HsLua.

-   Error handling has been reworked completely. The type of
    exceptions used and handled by HsLua is now exposed to the
    type system. The type `Lua` makes use of a default error type.
    Custom error handling can be implemented by using the `LuaE`
    type with an exception type that is an instance of class
    `LuaError`.

-   Renamed stack index helpers to `nth`, `nthTop`, `nthBottom`,
    `top`. The following have been *removed*: `stackTop`,
    `nthFromTop`, `nthFromBottom`.

-   Extracted raw Lua bindings into new package *lua*. This means
    that all cabal flags have been moved to package *lua* as well.
    Version lua-1.0.0 contained the `Foreign.Lua.Raw` hierarchy as
    present in hslua-1.3.0. See that package’s changelog for info
    on the additional modifications since then.

-   The module Foreign.Lua.Raw.Error was merged into the
    HsLua.Core.Error module.

-   The functions `getglobal` and `gettable` now return the Lua
    `Type` of the pushed value.

-   Extracted new packages:

    -   *hslua-core*: the package contains all modules from the
        *Core* sub-hierarchy.

    -   *hslua-classes*: typclasses *Peekable* and *Pushable* for
        pushing and pulling, as well as function calling.

    -   *tasty-hslua*: makes testing helpers available for reuse.

-   Moved *run* functions from Util to Core.Run.

-   Moved module Utf8 from the base level into Core.

-   Refactored code to expose Haskell functions to Lua:

    -   Removed functions `newCFunction`, `freeCFunction`. Use
        `pushHaskellFunction` instead, it takes care of garbage
        collection.

    -   Renamed typeclass `ToHaskellFunction` to `Exposable`,
        function `callFunc` to `invoke`. All these have been moved
        to *hslua-classes*.

    -   The type PreCFunction is now defined in package lua;
        HaskellFunction is defined in hslua-core.

    -   Changed `pushHaskellFunction` to only accept
        HaskellFunction arguments, move it to hslua-core.

    -   Removed helper functions `addfunction` and `addfield` from
        *Module*. Use documented functions and fields instead.

-   Added support for a “since” tag on documented functions;
    allows to mark the library version when a function was
    introduced in its present form.

## hslua-1.3.0.1

Released 2021-02-06.

-   Fixed build with GHC 9.0.1 (Simon Jakobi).

-   Improved test-suite; fixed memory leaks in some tests.

-   Moved CI to GitHub Actions.

## hslua-1.3.0

Released 2020-10-16.

-   Upgrade included Lua version to new bug-fix release 5.3.6. See
    the upstream documentation https://www.lua.org/bugs.html#5.3.5
    for the bugs which have been fixed.

-   Stop exporting `c_loaded_table` and `c_prelad_table` from
    module Foreign.Lua.Raw.Auxiliary. Both values are defined only
    if the flag `HARDCODE_REG_KEYS` is disabled, leading to
    compilation errors when the flag is enabled.

-   Add new function `peekStringy` to Peek module. It allows to
    peek a value of any `IsString` type from an UTF-8 encoded
    string.

-   Various improvements to the continuous integration setup,
    including cleanup of the config files, version bumps to the
    ghc/cabal versions used for testing, and running the linter in
    a dedicated GitHub Action.

## 1.2.0

Released 2020-08-15

-   New module `Foreign.Lua.Call`: the module offers an
    alternative method of exposing Haskell functions to Lua. The
    focus is on maintainability: types and marshaling methods are
    made explicit; the possibility of adding documentation and
    parameter names improves error messages and allows for
    automatic documentation extraction.

    Work on this module is ongoing; the interface is likely to
    change. Suggestions and feedback are welcome.

-   New types `Module`, `Field`, and new functions
    `registerModule`, `preloadModule`, `pushModule`, and `render`
    exported from `Foreign.Lua.Module`: this builds on the new
    `Call` module and allows the creation of documented modules as
    well as automatic generation of Markdown-formatted module
    documentation.

-   Export new items `nth` and `top` from Foreign.Lua.Core and
    Foreign.Lua. They are short-hands for `nthFromTop` and
    `stackTop`.

-   Performance improvements: Calling of Lua functions and
    creation of Haskell data wrapping userdata has been sped up by
    about 10%. This is mostly due to using of previously missed
    optimization opportunities.

-   All foreign imports have been moved to into the new
    `Foreign.Lua.Raw` module. This module will replace the current
    `Foreign.Lua.Core` module in the future and will be
    distributed as a separate package (likely starting with the
    2.0 release); the remaining parts of the current `Core` module
    will be promoted one level in the module hierarchy.

    The `Raw` module can be used whenever the full power of HsLua
    is not needed.

-   Error-signaling of API wrapper functions has been changed:
    instead of returning special integer values, functions now
    take an additional pointer argument, which is set to the
    status result of the computation.

    The `Failable` type in Core.Error is no longer needed and has
    been removed.

-   CI builds now include GHC 8.8 and GHC 8.10, ensuring that all
    GHC 8.\* versions are supported.

## hslua-1.1.2

Released 2020-06-27

-   Revert signature of function `pushList` to it’s proper 1.1
    value. This fixes a mistake which caused the 1.1.1 release to
    be in violation of the PVP versioning policy.

-   Module Foreign.Lua.Peek: add function `pushKeyValuePairs`
    (Alex Loomis).

## hslua-1.1.1

Released 2020-06-02

*WARNING*: This version does not conform to the PVP versioning
policy, due to a unintended signature change of function
`pushList`. It is recommended not to use this version.

-   New module Foreign.Lua.Push: provides functions which marshal
    and push Haskell values onto Lua’s stack.

    Most functions in Foreign.Lua.Types.Pushable are now defined
    using functions from this module.

-   New module Foreign.Lua.Peek: provides functions which
    unmarshal and retrieve Haskell values from Lua’s stack.
    Contrary to `peek` from Foreign.Lua.Types.Peekable, the peeker
    functions in this module will never throw errors, but use an
    `Either` type to signal retrieval failure.

    The error type `PeekError` should not be considered final and
    will likely be subject to change in later versions.

-   Module Foreign.Lua.Utf8: never throw errors when decoding
    UTF-8 strings. Invalid UTF-8 input bytes no longer cause
    exceptions, but are replaced with the Unicode replacement
    character U+FFFD.

-   Fixed missing and faulty Haddock documentation.

-   Fixed a bug which caused unnecessary use of strings to
    represent floating point numbers under certain configurations.

## hslua-1.1.0

Released 2020-03-25.

**WARNING:** The changes in this release are experimental. It is
recommended to skip this release unless the newly introduced
features are required.

-   Allow custom error handling: conversion of Lua errors to
    Haskell exceptions and back is made configurable. Users can
    define their own exception/error handling strategies, even
    opening up the option to pass arbitrary exceptions through
    Lua.

    -   New types exported from `Foreign.Lua.Types`:

        -   `ErrorConversion`: defines the ways in which
            exceptions and errors are handled and converted.
        -   `LuaEnvironment`: environment in which Lua
            computations are evaluated. Contains the Lua
            interpreter state and the error conversion strategy.

    -   The environment of the `Lua` type is changed from a plain
        Lua `State` to the above mentioned `LuaEnvironment`.

    -   New functions `run'` is exported from `Foreign.Lua.Util`
        and `Foreign.Lua`: it is analogous to `run`, but allows to
        run computations with a custom error conversion strategy.

    -   New function `runWithConverter` exported from
        `Foreign.Lua.Core.Types` and `Foreign.Lua.Core`; like
        `run'`, but takes a custom state.

    -   New function `unsafeRunWith` exported from
        `Foreign.Lua.Core.Types` and `Foreign.Lua.Core`; runs a
        computation without proper error handling.

    -   New function `errorConversion` exported from
        `Foreign.Lua.Core.Types` and `Foreign.Lua.Core`: extract
        the error conversion strategy from the Lua type.

    -   New function `throwErrorAsException` exported from
        `Foreign.Lua.Core.Error` and `Foreign.Lua.Core`: throws a
        Lua error as Haskell exception, using the current error
        conversion strategy.

-   Function `runWith` is moved from module `Foreign.Lua.Core` to
    `Foreign.Lua.Util`.

-   The module `Foreign.Lua.Utf8` is now exported.

## hslua-1.0.3.2

Released 2019-08-21.

-   Added flag to use hardcoded values for registry keys: The
    names of the registry keys used to store package information
    are available as CPP values from file lauxlib.h since Lua
    5.3.4; compiling HsLua against older Lua versions was not
    possible, as those values were expected to exist.

    The respective values are now hardcoded into HsLua, and a new
    flag `hardcode-reg-key` is introduced, which will cause the
    use of these hardcoded values instead of those defined in
    lauxlib.h. Using this flag makes it possible to compile hslua
    against all Lua 5.3.\* versions.

-   Added missing C files for benchmarking to list of
    *extra-source-files*.

## hslua-1.0.3.1

Released 2019-05-08.

-   Prevent filenames being treated as strings in debug messages.
    Lua’s `loadbuffer` takes a `source` description as an
    argument, which is used for debug messages. The `loadfile`
    function now adds a special prefix (`@`) to `source`, thus
    marking it as a filename.

## hslua-1.0.3

Released 2019-05-04.

-   New module `Foreign.Lua.Module`, containing helper functions
    to define and load modules from Haskell.

-   Improve documentation of `open<lib>` (many thanks to Christian
    Charukiewicz.)

## hslua-1.0.2

Released 2019-01-05.

-   Fixed cross-compilation: placement of C import declarations
    were fixed, thereby resolving issues with cross-compilation.
    (Vanessa McHale and Faraz Maleknia)

-   Added .gitattributes file, fixing the wrong language
    classification of the GitHub repository. (Vanessa McHale)

-   Improved `toHaskellFunction` documentation. The documentation
    is now more specific on which Haskell exceptions are caught
    and which will lead to crashes.

## hslua-1.0.1

-   Exposed more functions from Lua’s `lauxlib` library:

    -   `getmetafield`,
    -   `getmetatable'`,
    -   `getsubtable`, and
    -   `traceback`.

    The function `getsubtable` is a reimplementation instead of a
    wrapper to the C function for simplicity (thereby avoiding
    additional C wrappers).

-   Fixed tests for GHC 8.6 by no longer depending on failable
    pattern matching.

## hslua-1.0.0

### New features

-   Error handling at language borders has been vastly improved
    and is now mostly automatic. Haskell’s
    `Foreign.Lua.Exception`s are transformed into Lua errors and
    *vice versa*. Lua-side wrappers are no longer necessary.

-   Haskell functions are no longer pushed as userdata by
    `pushHaskellFunction`, but as C functions. This simplifies
    tasks where Lua expects true function objects object (for
    example when looking for module loaders).

-   Added stack instance for

    -   Data.Set.Set,
    -   Integer,
    -   Int,
    -   Float, and
    -   Double.

    Instances for numbers fall back to strings when the
    representation as a Lua number would cause a loss of
    precision.

-   Haskell functions pushed with `pushHaskellFunction` can now be
    garbage collected by Lua without having to call back into
    Haskell. The callback into Haskell by the GC had previously
    caused programs to hang in some situations.

-   Bindings to more Lua C API functions and macros: `isinteger`,
    `load`, `loadbuffer`, and `pushglobaltable`.

-   Any Haskell value can be pushed to the Lua stack as userdata
    via `pushAny` and retrieved via `peekAny`. Additional
    functions are provided to setup the userdata metatable.

-   The C preprocessor constants `LUA_LOADED_TABLE` and
    `LUA_PRELOAD_TABLE` are made available as
    `loadedTableRegistryField` and `preloadTableRegistryField`,
    respectively.

-   Additional small helper functions:

    -   `peekRead` – read value from a string.
    -   `popValue` – peek value at the top of the Lua stack, then
        remove it from the stack regardless of whether peeking was
        successful or not.

### Naming

-   The *Lua* prefix was removed from types (`State`, `Integer`,
    `Number`, `Exception`) and the respective infix from functions
    (`try`, `run`, `runWith`, `runEither`). HsLua should be
    imported qualified to avoid name collisions.

-   Terminology now consistently uses *exception* to refer to
    Haskell exceptions, and *error* for Lua errors; function names
    changed accordingly (`throwException`, `catchException`,
    `withExceptionMessage`).

-   Module *Foreign.Lua.Api* was renamed to *Foreign.Lua.Core*.

-   *Foreign.Lua.lerror* was renamed to *Foreign.Lua.error*.

-   Typeclass *ToLuaStack* was renamed to *Pushable*.

-   Typeclass *FromLuaStack* was renamed to *Peekable*.

-   Cabal flag *use-pkgconfig* was renamed to *pkg-config* (which
    is the flag name used by other projects such a zlib).

### Type signatures

-   The return value of `lua_newuserdata` is *CSize* (was *CInt*).

-   Table index parameter in `rawgeti` and `rawseti` must be of
    type *LuaInteger*, but were of type *Int*.

-   The number of upvalues passed to `pushcclosure` must be of
    type *NumArgs*.

-   `Lua.error` has type *Lua NumResults*, simplifying its use in
    HaskellFunctions.

-   Retrieval functions which can fail, i.e. `tocfunction`,
    `tointeger`, `tonumber`, `tostring`, `tothread`, and
    `touserdata`, use the *Maybe* type to indicate success or
    failure, avoiding the need to perform additional checks.

### Removed Features

-   Support for Lua versions before 5.3 has been dropped.

-   Support for GHC 7.8 has been dropped.

-   `wrapHaskellFunction` has been made internal and is no longer
    exported.

### Changed behavior

-   Peekable instances for numbers and strings became more
    forgiving. Peeking of basic types now follows Lua’s default
    conversion rules:

    -   numbers can be given as strings, and *vice versa*;
    -   any value can be converted into a boolean – only `nil` and
        `false` are peeked as `False`, all other as `True`.

### Other

-   Many internal improvements and additions such as a
    benchmarking suite, code cleanups, better tests, etc.

## hslua-0.9.5.2

-   Relaxed upper bound on *exceptions*. Again.

## hslua-0.9.5.1

-   Relaxed upper bound on *exceptions*.

## hslua-0.9.5

-   Provide Optional as a replacement for OrNil. Exports of the
    latter have been fixed.
-   Provide utility function `raiseError`: Its argument will be
    thrown as an error in Lua.
-   Add `modifyLuaError`: The function lives in Foreign.Lua.Error
    and allows to alter error messages. This is most useful for
    amending errors with additional information.
-   Fixed a bug in `toList` which left a element on the stack if
    deserializing that element lead to an error. This also
    affected the FromLuaStack instance for lists.
-   Fixed a bug in `pairsFromTable` which left a key-value pair on
    the stack if either of them could not be read into the
    expected type. This also affected the FromLuaStack instance
    for Map.

## hslua-0.9.4

-   Make Lua an instance of MonadMask: MonadMask from
    Control.Monad.Catch allows to mask asynchronous exceptions.
    This allows to define a finalizer for Lua operations.
-   Add functions and constants to refer to stack indices: The
    functions `nthFromBottom`, `nthFromTop` as well as the
    constants `stackTop` and `stackBottom` have been introduced.
    Numeric constants are less clear, and named constants can aid
    readability.
-   Add type OrNil: This type can be used when dealing with
    optional arguments to Lua functions.
-   Add function absindex: it converts the acceptable index `idx`
    into an equivalent absolute index (that is, one that does not
    depend on the stack top). The function calls `lua_absindex`
    when compiled with Lua 5.2 or later; for Lua 5.1, it is
    reimplemented in Haskell.
-   Functions in `tasty` which have been deprecated have been
    replaced with non-deprecated alternatives.

## hslua-0.9.3

-   Re-export more FunctionCalling helpers in `Foreign.Lua`: The
    typeclass `ToHaskellFunction` and the helper function
    `toHaskellFunction` are useful when working with functions.
    Importing them separately from `Foreign.Lua.FunctionCalling`
    was an unnecessary burden; they are therefor now re-exported
    by the main module.
-   Export registry-relatd constants `refnil` and `noref`: The
    constants are related to Lua’s registry functions (`ref` and
    `unref`).
-   Add helper to convert functions into CFunction: A new helper
    `wrapHaskellFunction` is provided. It expects a
    HaskellImportedFunction userdata (as produced by
    `pushHaskellFunction`) on top of the stack and replaces it
    with a C function. The new function converts error values
    generated with `lerror` into Lua errors, i.e. it calls
    `lua_error`.
-   Add utility function `setglobal'`: It works like `setglobal`,
    but works with packages and nested tables (dot-notation only).

## hslua-0.9.2

-   Add cabal flag ‘export-dynamic’: Default behavior is to
    include all symbols in the dynamic symbol table, as this
    enables users to load dynamic lua libraries. However, it is
    sometimes desirable to disable, e.g., when compiling a fully
    static binary. See jgm/pandoc#3986.

## hslua-0.9.1

-   Increase user-friendlyness of error messages: The error
    message returned by `toHaskellFunction` hinted at the fact
    that the failing function is a Haskell function. This is
    mostly unnecessary information and might have confused users.

## hslua-0.9.0

-   Added cabal flag to allow fully safe garbage collection: Lua
    garbage collection can occur in most of the API functions,
    even in those usually not calling back into haskell and hence
    marked as optimizable. The effect of this is that finalizers
    which call Haskell functions will cause the program to hang. A
    new flag `allow-unsafe-gc` is introduced and enabled by
    default. Disabling this flag will mark more C API functions as
    potentially calling back into Haskell. This has a serious
    performance impact.
-   `FromLuaStack` and `ToLuaStack` instances for lazy ByteStrings
    are added.
-   None-string error messages are handled properly: Lua allows
    error messages to be of any type, but the haskell error
    handlers expected string values. Tables, booleans, and other
    non-string values are now handled as well and converted to
    strings.

## hslua-0.8.0

-   Use newtype definitions instead of type aliases for LuaNumber
    and LuaInteger. This makes it easier to ensure the correct
    numeric instances in situations where Lua might have been
    compiled with 32-bit numbers.
-   Instances of `FromLuaStack` and `ToLuaStack` for `Int` are
    removed. The correctness of these instances cannot be
    guaranteed if Lua was compiled with a non-standard integer
    type.

## hslua-0.7.1

-   The flag `lua_32bits` was added to allow users to compile Lua
    for 32-bit systems.
-   When reading a list, throw an error if the lua value isn’t a
    table instead of silently returning an empty list.

## hslua-0.7.0

-   Tuples from pairs to octuples have been made instances of
    `FromLuaStack` and `ToLuaStack`.
-   New functions `dostring` and `dofile` are provided to load and
    run strings and files in a single step.
-   `LuaStatus` was renamed to `Status`, the *Lua* prefix was
    removed from its type constructors.
-   The constructor `ErrFile` was added to `Status`. It is
    returned by `loadfile` if the file cannot be read.
-   Remove unused FFI bindings and unused types, including all
    functions unsafe to use from within Haskell and the library
    functions added with 0.5.0. Users with special requirements
    should define their own wrappers and raw bindings.
-   The module *Foreign.Lua.Api.SafeBindings* was merge into
    *Foreign.Lua.Api.RawBindings*.
-   FFI bindings are changed to use newtypes where sensible, most
    notably `StackIndex`, `NumArgs`, and `NumResults`, but also
    the newly introduced newtypes `StatusCode`, `TypeCode`, and
    `LuaBool`.
-   Add functions `tointegerx` and `tonumberx` which can be used
    to get and check values from the stack in a single step.
-   The signature of `concat` was changed from `Int -> Lua ()` to
    `NumArgs -> Lua ()`.
-   The signature of `loadfile` was changed from
    `String -> Lua Int` to `String -> Lua Status`.
-   The type `LTYPE` was renamed to `Type`, its constructors were
    renamed to follow the pattern `Type<Typename>`. `LuaRelation`
    was renamed to `RelationalOperator`, the *Lua* prefix was
    removed from its constructors.
-   Add function `tolist` to allow getting a generic list from the
    stack without having to worry about the overlapping instance
    with `[Char]`.

## hslua-0.6.0

-   Supported Lua Versions now include Lua 5.2 and Lua 5.3. LuaJIT
    and Lua 5.1 remain supported as well.
-   Flag `use-pkgconfig` was added to allow discovery of library
    and include paths via pkg-config. Setting a specific Lua
    version flag now implies `system-lua`. (Sean Proctor)
-   The module was renamed from `Scripting.Lua` to `Foreign.Lua`.
    The code is now split over multiple sub-modules. Files
    processed with hsc2hs are restricted to Foreign.Lua.Api.
-   A `Lua` monad (reader monad over LuaState) is introduced.
    Functions which took a LuaState as their first argument are
    changed into monadic functions within that monad.
-   Error handling has been redesigned completely. A new
    LuaException was introduced and is thrown in unexpected
    situations. Errors in lua which are leading to a `longjmp` are
    now caught with the help of additional C wrapper functions.
    Those no longer lead to uncontrolled program termination but
    are converted into a LuaException.
-   `peek` no longer returns `Maybe a` but just `a`. A
    LuaException is thrown if an error occurs (i.e. in situtations
    where Nothing would have been returned previously).
-   The `StackValue` typeclass has been split into `FromLuaStack`
    and `ToLuaStack`. Instances not satisfying the law
    `x == push x *> peek (-1)` have been dropped.
-   Documentation of API functions was improved. Most docstrings
    have been copied from the official Lua manual, enriched with
    proper markup and links, and changed to properly describe
    hslua specifics when necessary.
-   Example programs have been moved to a separate repository.
-   Unused files were removed. (Sean Proctor)

## hslua-0.5.0

-   New raw functions for `luaopen_base`, `luaopen_package`,
    `luaopen_string`, `luaopen_table`, `luaopen_math`,
    `luaopen_io`, `luaopen_os`, `luaopen_debug` and their
    high-level wrappers (with names `openbase`, `opentable` etc.)
    implemented.
-   Remove custom versions of `loadfile` and `loadstring`.
-   Drop support for GHC versions \< 7.8, avoid compiler warnings.
-   Ensure no symbols are stripped when linking the bundled lua
    interpreter.
-   Simplify `tostring` function definition. (Sean Proctor)
-   Explicitly deprecate `strlen`. (Sean Proctor)
-   Add links to lua documentation for functions wrapping the
    official lua C API. (Sean Proctor).

## hslua-0.4.1

-   Bugfix(#30): `tolist` wasn’t popping elements of the list from
    stack.

## hslua-0.4.0

-   `pushstring` and `tostring` now uses `ByteString` instead of
    `[Char]`.
-   `StackValue [Char]` instance is removed,
    `StackValue ByteString` is added.
-   `StackValue a => StackValue [a]` instance is added. It pushes
    a Lua array to the stack. `pushlist`, `islist` and `tolist`
    functions are added.
-   Type errors in Haskell functions now propagated differently.
    See the `Scripting.Lua` documentation for detailed
    explanation. This should fix segfaults reported several times.
-   `lua_error` function is removed, it’s never safe to call in
    Haskell.

Related issues and pull requests: #12, #26, #24, #23, #18.

## hslua-0.3.14

-   Pkgconf-based setup removed. Cabal is now using
    `extra-libraries` to link with Lua.
-   `luajit` flag is added to link hslua with LuaJIT.

## hslua-0.3.13

-   Small bugfix related with GHCi running under Windows.

## hslua-0.3.12

-   `pushrawhsfunction` and `registerrawhsfunction` functions are
    added.
-   `apicheck` flag is added to Cabal package to enable Lua API
    checking. (useful for debugging)

## hslua-0.3.11

-   `luaL_ref` and `luaL_unref` functions are added.

  [PVP Versioning]: https://pvp.haskell.org
