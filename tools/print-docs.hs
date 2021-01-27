#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , hslua
             , hslua-module-path
             , text
-}

import Data.Text
import Foreign.Lua.Module (render)
import Foreign.Lua.Module.Path (documentedModule)

main :: IO ()
main = putStrLn . unpack $ render documentedModule
