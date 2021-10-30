{-|
Module      : Lua.LPeg
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Haskell bindings to the LPeg Lua package.
-}
module Lua.LPeg
  ( luaopen_lpeg_ptr
  , lpeg_searcher
  ) where

import Foreign.C
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)
import Lua

-- | Pointer to the placeholder library loader. Trying to load the
-- library will result in an error.
foreign import ccall unsafe "placeholder.c &luaopen_lpeg"
  luaopen_lpeg_ptr :: CFunction

-- | Placeholder package searcher to be used with @package.searchers@),
-- just for the "lpeg" module. Returns @nil@ on most inputs, but pushes
-- adds a message about the module being unavailable when called on the
-- search key "lpeg".
lpeg_searcher :: PreCFunction
lpeg_searcher l = do
  alloca $ \lenPtr -> do
    cstr <- lua_tolstring l 1 lenPtr
    if cstr == nullPtr
      then 1 <$ lua_pushnil l
      else do
        cstrLen <- peek lenPtr
        pkg <- peekCStringLen (cstr, fromIntegral cstrLen)
        if pkg /= moduleName
          then 1 <$ lua_pushnil l
          else 1 <$ withCString msg (lua_pushstring l)
 where
  msg = "\n\tlpeg was configured to be excluded from the binary."

-- | Name under which the module is loaded.
moduleName :: String
moduleName = "lpeg"
