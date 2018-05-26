# hslua – Lua interpreter interface for Haskell

[![Build Status]](https://travis-ci.org/hslua/hslua)
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Hackage]](https://hackage.haskell.org/package/hslua)

Hslua provides bindings, wrappers, types, and helper functions to bridge haskell
and lua.

[Build Status]: https://travis-ci.org/hslua/hslua.svg?branch=master
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/master?svg=true
[Hackage]: http://img.shields.io/hackage/v/hslua.svg


Overview
--------

[Lua](https://lua.org) is a small, well-designed, embeddable scripting language.
It has become the de-facto default to make programs extensible and is widely
used everywhere from servers over games and desktop applications up to security
software and embedded devices. This package provides Haskell bindings to Lua,
enable coders to embed the language into their programs, making them scriptable.

HsLua ships with batteries included and includes the most recent Lua version.
However, cabal flags make it easy to swap this out in favor of a Lua version
already installed on the host system. It supports the versions 5.1, 5.2, 5.3,
and LuaJIT.


Interacting with Lua
--------------------

HsLua provides the `Lua` type to define Lua operations. The operations are
executed by calling `runLua`. A simple "Hello, World" program, using the Lua
`print` function, is given below:

``` haskell
import Foreign.Lua

main :: IO ()
main = runLua prog
  where
    prog :: Lua ()
    prog = do
      openlibs  -- load lua libraries so we can use 'print'
      callFunc "print" "Hello, World!"
```

### The Lua stack

Lua's API is stack-centered: most operations involve pushing values to the stack
or receiving items from the stack. E.g., calling a function is performed by
pushing the function onto the stack, followed by the function arguments in the
order they should be passed to the function. The API function `call` then
invokes the function with given numbers of arguments, pops the function and
parameters of the stack, and pushes the results.

    ,----------.
    |  arg 3   |
    +----------+
    |  arg 2   |
    +----------+
    |  arg 1   |
    +----------+                  ,----------.
    | function |    call 3 1      | result 1 |
    +----------+   ===========>   +----------+
    |          |                  |          |
    |  stack   |                  |  stack   |
    |          |                  |          |

Manually pushing and pulling arguments can become tiresome, so HsLua makes
function calling simple by providing `callFunc`. It uses type-magic to allow
different numbers of arguments. Think about it as having the signature

    callFunc :: String -> a1 -> a2 -> … -> res

where the arguments `a1, a2, …` must be of a type which can be pushed to the Lua
stack, and the result-type `res` must be constructable from a value on the Lua
stack.

### Getting values from and to the Lua stack

Conversion between Haskell and Lua values is governed by two type classes:

``` haskell
-- | A value that can be read from the Lua stack.
class FromLuaStack a where
  -- | Check if at index @n@ there is a convertible Lua value and
  --   if so return it.  Throws a @'LuaException'@ otherwise.
  peek :: StackIndex -> Lua a
```

and

``` haskell
-- | A value that can be pushed to the Lua stack.
class Pushable a where
  -- | Pushes a value onto Lua stack, casting it into meaningfully
  --   nearest Lua type.
  push :: a -> Lua ()
```

Many basic data types (except for numeric types, see the FAQ) have instances for
these type classes. New instances can be defined for custom types using the
functions in `Foreign.Lua.Api` (also exported in `Foreign.Lua`).


Build flags
-----------

The following cabal build flags are supported:

- `system-lua`: Use the locally installed Lua version instead of the version
  shipped as part of HsLua.

- `use-pkgconfig`: Use `pkg-config` to discover library and include paths. This
  is used only when the `system-lua` flag is set or implied.

- `lua501`: Build against Lua 5.1; this implies the flag `system-lua` as well.

- `lua502`: Build against Lua 5.2; this implies the flag `system-lua` as well.

- `luajit`: Build against LuaJIT; this implies the flag `system-lua` as well.

- `allow-unsafe-gc`: Allow optimizations which make Lua's garbage collection
  potentially unsafe; haskell finalizers must be handled with extreme care. This
  is *enabled* per default, as this is rarely a problem in practice.

- `apicheck`: Compile Lua with its API checks enabled.

- `lua_32bits`: Compile Lua for a 32-bits system (e.g., i386, PowerPC G4).


### Example: using a different lua version

To use a system-wide installed Lua/LuaJIT when linking hslua as a dependency,
build/install your package using `--constraint="hslua +system-lua"` or for
LuaJIT: `--constraint="hslua +luajit"`. For example, you can install Pandoc with
hslua that uses system-wide LuaJIT like this:

``` sh
cabal install pandoc --constraint="hslua +system-lua +luajit"
```

or with stack:

``` sh
stack install pandoc --flag=hslua:luajit
```


FAQ
---

**Is anybody using this?** Absolutely. E.g., [Pandoc](https://pandoc.org), the
universal document converter, is written in Haskell and includes a Lua
interpreter, enabling programmatic modifications of documents via Lua.
Furthermore, custom output formats can be defined via Lua scripts. This has been
used in [pandoc-scholar](https://github.com/pandoc-scholar/pandoc-scholar)
([paper](https://peerj.com/articles/cs-112/)) to allow for semantically enriched
scholarly articles.

**Where are the coroutine related functions?** Yielding from a coroutine works
via `longjmp`, which plays very badly with Haskell's RTS. Tests to get
coroutines working with HsLua were unsuccessful. No coroutine related functions
are exported from the default module for that reason. However, raw bindings to
the C API functions are still provided in `Foreign.Lua.RawBindings`. If you get
coroutines to work, or just believe that there should be wrapper functions for
other reasons, we'd love to hear from you.

**Why are there no predefined stack instances for default numerical types?**
HsLua defines instances for the `FromLuaStack` and `Pushable` type-classes
only if the following law holds: `return x == push x *> peek x`. Lua can be
compiled with customized number types, making it impossible to verify the
correctness of the above equation. Furthermore, instances for numerical types
can be based on those of LuaInteger and LuaNumber and are easy to write.
Therefor hslua doesn't provide any such instances.
