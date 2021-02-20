{-# LANGUAGE CPP #-}
{-|
Module      : Foreign.Lua.Raw.Functions
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, CPP

Haskell bindings to Lua C API functions.

The exposed functions correspond closely to the respective C Lua API
functions. However, C API functions which can throw Lua errors are not
exported directly, as any errors would crash the program. Non-error
throwing @hslua_@ versions are provided instead. The @hslua@ ersatz
functions have worse performance than the original.

Some of the Lua functions may, directly or indirectly, call a Haskell
function, and trigger garbage collection, rescheduling etc. These
functions are always imported safely (i.e., with the @safe@ keyword).

However, all function can trigger garbage collection. If that can lead
to problems, then the package should be configured without flag
@allow-unsafe-gc@.
-}
module Foreign.Lua.Raw.Functions
  {-# DEPRECATED "Use Foreign.Lua.Functions instead" #-}
  (module Foreign.Lua.Functions)
where

import Foreign.Lua.Functions
