# hslua -- Lua interpreter interface for Haskell

[![Build Status](https://travis-ci.org/osa1/hslua.svg?branch=master)](https://travis-ci.org/osa1/hslua)
[![Coverage Status](https://coveralls.io/repos/osa1/hslua/badge.svg?branch=master&service=github)](https://coveralls.io/github/osa1/hslua?branch=master)
[![Hackage](http://img.shields.io/hackage/v/hslua.svg)](https://hackage.haskell.org/package/hslua)

To use system-wide installed Lua/LuaJIT when linking hslua as a dependency, build/install your package using `--constraint="hslua +system-lua"` or for LuaJIT: `--constraint="hslua +system-lua +luajit"`. For example, you can install Pandoc with hslua that uses system-wide LuaJIT like this:

```
cabal install pandoc --constraint="hslua +system-lua +luajit"
```

(Note that `-fluajit` flag is added with hslua 0.3.14)
