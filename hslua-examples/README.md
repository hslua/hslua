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


lua-version
-----------

A simple program which uses Lua library functions and Lua
variables to print the Lua version against which the program was
linked.

haskellfun
----------

Demo how functions written in Haskell can be exposed to Lua.
Includes a short Lua script which makes use of these functions.

callbacks
---------

Program that demonstrates how Haskell callbacks can be passed to
Lua, and how Lua callbacks can be collected and called from
Haskell.

err_prop
--------

Demonstrates how errors propagate in HsLua programs. This consists
of two parts: the Haskell program, and a short Lua script.

lualib_in_haskell
-----------------

Lua can make use of dynamically loaded libraries. This shows how
such a library can be created with HsLua, exposing functions
written in Haskell to Lua.
