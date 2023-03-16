hslua-repl
==========

An embeddable, isocline-based Lua REPL.

Example
-------

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
import HsLua.Core  as Lua (Exception, openlibs, run)
import HsLua.REPL (defaultREPLConfig, startREPL)

-- | Run a default Lua interpreter.
main :: IO ()
main = run @Lua.Exception $ do
  startREPL defaultREPLConfig
```
