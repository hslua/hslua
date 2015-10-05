{-# LANGUAGE ScopedTypeVariables #-}

module LibArith where

import Data.Maybe
import Foreign.C.Types

import Scripting.Lua
import Scripting.Lua.Raw

foreign export ccall
  add :: LuaState -> IO CInt

add :: LuaState -> IO CInt
add l = do
  i1 :: LuaNumber <- fromJust `fmap` peek l 1
  i2 :: LuaNumber <- fromJust `fmap` peek l 2
  push l (i1 + i2 :: LuaNumber)
  return 1
