{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Test.Tasty.Lua.Module
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires TemplateHaskell

Tasty Lua module, providing the functions necessary to write tasty tests
in Lua scripts.
-}
module Test.Tasty.Lua.Module
  ( pushModule )
where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Foreign.Lua (Lua, NumResults, Status (OK), dostring, throwTopMessage)

-- | Tasty Lua script
tastyScript :: ByteString
tastyScript = $(embedFile "tasty.lua")

-- | Push the Aeson module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  result <- dostring tastyScript
  if result == OK
    then return 1
    else throwTopMessage
{-# INLINABLE pushModule #-}
