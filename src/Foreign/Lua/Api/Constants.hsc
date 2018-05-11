{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017-2018 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-|
Module      : Foreign.Lua.Api.Constants
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Lua constants
-}
module Foreign.Lua.Api.Constants
  ( multret
  , registryindex
  , refnil
  , noref
#if LUA_VERSION_NUMBER < 502
  , environindex
  , globalsindex
#endif
  ) where

import Foreign.Lua.Api.Types

#include "lua.h"
#include "lauxlib.h"

-- | Alias for C constant @LUA_MULTRET@. See
-- <https://www.lua.org/manual/5.3/#lua_call lua_call>.
multret :: NumResults
multret = NumResults $ #{const LUA_MULTRET}

-- | Alias for C constant @LUA_REGISTRYINDEX@. See
-- <https://www.lua.org/manual/5.3/#3.5 Lua registry>.
registryindex :: StackIndex
registryindex = StackIndex $ #{const LUA_REGISTRYINDEX}

-- | Value signaling that no reference was created.
refnil :: Int
refnil = #{const LUA_REFNIL}

-- | Value signaling that no reference was found.
noref :: Int
noref = #{const LUA_NOREF}

#if LUA_VERSION_NUMBER < 502
-- | Alias for C constant @LUA_ENVIRONINDEX@. See
-- <https://www.lua.org/manual/5.1/#3.3 pseudo-indices>.
environindex :: StackIndex
environindex = StackIndex $ #{const LUA_ENVIRONINDEX}

-- | Alias for C constant @LUA_GLOBALSINDEX@. See
-- <https://www.lua.org/manual/5.1/#3.3 pseudo-indices>.
globalsindex :: StackIndex
globalsindex = StackIndex $ #{const LUA_GLOBALSINDEX}
#endif
