HsLua examples
==============

[![Build status][GitHub Actions badge]][GitHub Actions]
[![AppVeyor Status]](https://ci.appveyor.com/project/tarleb/hslua-r2y18)
[![Hackage]](https://hackage.haskell.org/package/hslua-examples)

Example programs showcasing the HsLua framework.

[GitHub Actions badge]: https://img.shields.io/github/workflow/status/hslua/hslua/CI.svg?logo=github
[GitHub Actions]: https://github.com/hslua/hslua/actions
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/ldutrilgxhpcau94/branch/main?svg=true
[Hackage]: https://img.shields.io/hackage/v/hslua-core.svg

run-lua
-----------

A simple program which uses Lua to calculate and print Fibonacci
numbers. It demonstrates how a Lua script can be embedded and
executed.

print-version
-------------

Demonstrates the use of the the low-level C API functions from the
`lua` package. Prints the Lua version.

wishlist
----------

The code for [Santa's Little Lua Scripts][SLLS].

low-level-factorial
-------------------

Calculate integer factorials in Haskell, allowing for results
which don't fit into a normal Lua integer. Uses only the low-level
C API functions from the `lua` package.

[SLLS]: https://hslua.org/santas-little-lua-scripts.html
