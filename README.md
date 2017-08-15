# hslua – Lua interpreter interface for Haskell

[![Build Status]](https://travis-ci.org/osa1/hslua)
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Coverage Status]](https://coveralls.io/github/osa1/hslua?branch=master)
[![Hackage]](https://hackage.haskell.org/package/hslua)

Hslua provides bindings, wrappers, types, and helper functions to bridge haskell
and lua.

[Build Status]: https://travis-ci.org/osa1/hslua.svg?branch=master
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/master?svg=true
[Coverage Status]: https://coveralls.io/repos/osa1/hslua/badge.svg?branch=master&service=github
[Hackage]: http://img.shields.io/hackage/v/hslua.svg


Using a different lua version
-----------------------------

To use system-wide installed Lua/LuaJIT when linking hslua as a dependency,
build/install your package using `--constraint="hslua +system-lua"` or for
LuaJIT: `--constraint="hslua +system-lua +luajit"`. For example, you can install
Pandoc with hslua that uses system-wide LuaJIT like this:

``` sh
cabal install pandoc --constraint="hslua +system-lua +luajit"
```


FAQ
---

**Where are the coroutine related functions?** Yielding from a coroutine works
via `longjmp`, which plays very badly with Haskell's RTS. Tests to get
coroutines working with HsLua were unsuccessful. No coroutine related functions
are exported from the default module for that reason. However, raw bindings to
the C API functions are still provided in `Foreign.Lua.RawBindings`. If you get
coroutines to work, or just believe that there should be wrapper functions for
other reasons, we'd love to hear from you.

**Why are there no predefined stack instances for default numerical types?**
HsLua defines instances for the `FromLuaStack` and `ToLuaStack` type-classes
only if the following law holds: `return x == push x *> peek x`. Lua can be
compiled with customized number types, making it impossible to verify the
correctness of the above equation. Furthermore, instances for numerical types
can be based on those of LuaInteger and LuaNumber and are easy to write.
Therefor hslua doesn't provide any such instances.
