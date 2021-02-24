{-|
Module      : Foreign.Lua.Ersatz
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta

Ersatz functions for Lua API items which may, directly or indirectly,
throw a Lua error.
-}
module Foreign.Lua.Ersatz
  ( -- * Get functions (Lua -> stack)
    hslua_gettable
  , hslua_getglobal
    -- * Set functions (stack -> Lua)
  , hslua_settable
  , hslua_setglobal
    -- * Misc
  , hslua_error
  , hslua_next
  , hslua_concat
  , hslua_compare
    -- * Auxiliary Library
  , hsluaL_newstate
  , hsluaL_tolstring
  )where

import Foreign.Lua.Ersatz.Functions
import Foreign.Lua.Ersatz.Auxiliary (hsluaL_newstate, hsluaL_tolstring)
