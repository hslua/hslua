#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , hslua
             , hslua-module-paths
             , text
-}

import Data.Text
import Foreign.Lua.Module (render)
import Foreign.Lua.Module.Paths (documentedModule)

main :: IO ()
main = putStrLn . unpack $ render documentedModule
