{-|
Module      : Lua.LPeg
Copyright   : © 2021-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Haskell bindings to the LPeg Lua package.
-}
module Lua.LPeg
  ( luaopen_lpeg_ptr
  , luaopen_re_ptr
  , lpeg_searcher
  ) where

import Foreign.C
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)
import Lua

-- | Pointer to the placeholder lpeg library loader. Trying to load the
-- library will result in an error.
foreign import ccall unsafe "placeholder.c &luaopen_lpeg"
  luaopen_lpeg_ptr :: CFunction

-- | Pointer to the placeholder re library loader. Trying to load the
-- library will result in an error.
foreign import ccall unsafe "placeholder.c &luaopen_re"
  luaopen_re_ptr :: CFunction

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
        case pkg of
          "lpeg" -> 1 <$ withCString msg (lua_pushstring l)
          "re"   -> 1 <$ withCString msg (lua_pushstring l)
          _      -> 1 <$ lua_pushnil l
 where
  msg = "\n\tlpeg was configured to be excluded from the binary."
