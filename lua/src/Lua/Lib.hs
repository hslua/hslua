{-|
Module      : Lua.Lib
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, CPP

Lua standard libraries.
-}
module Lua.Lib
  ( luaopen_base
  , luaopen_table
  , luaopen_io
  , luaopen_os
  , luaopen_string
  , luaopen_math
  , luaopen_debug
  , luaopen_package
  )
where

import Lua.Types (CFunction)

-- * Lua Libraries

-- | Pointer to function opening the base library.
foreign import ccall unsafe "lualib.h &luaopen_base"
  luaopen_base :: CFunction

-- | Pointer to function opening the table library.
foreign import ccall unsafe "lualib.h &luaopen_table"
  luaopen_table :: CFunction

-- | Pointer to function opening the io library.
foreign import ccall unsafe "lualib.h &luaopen_io"
  luaopen_io :: CFunction

-- | Pointer to function opening the os library.
foreign import ccall unsafe "lualib.h &luaopen_os"
  luaopen_os :: CFunction

-- | Pointer to function opening the string library.
foreign import ccall unsafe "lualib.h &luaopen_string"
  luaopen_string :: CFunction

-- | Pointer to function opening the math library.
foreign import ccall unsafe "lualib.h &luaopen_math"
  luaopen_math :: CFunction

-- | Pointer to function opening the debug library.
foreign import ccall unsafe "lualib.h &luaopen_debug"
  luaopen_debug :: CFunction

-- | Pointer to function opening the package library.
foreign import ccall unsafe "lualib.h &luaopen_package"
  luaopen_package :: CFunction
