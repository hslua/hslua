hslua-cli
=========

Library that allows to embed a standalone Lua interpreter into a
larger program. The provided command-line interface is mostly
compatible with that of the default `lua` executable that ships
with Lua.

Example
-------

``` haskell
import HsLua.Core  as Lua (Exception, openlibs, run)
import HsLua.CLI (Settings (..), runStandalone)

-- | Run a default Lua interpreter.
main :: IO ()
main = do
  let settings = Settings
        { settingsVersionInfo = "\nembedded in MyProgram"
        , settingsRunner = \action -> run $ do
            openlibs
            action
        }
  runStandalone @Lua.Exception settings
```
