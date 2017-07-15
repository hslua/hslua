{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017 Albert Krewinkel

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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-|
Module      : Foreign.Lua.Types.Core
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, GeneralizedNewtypeDeriving

The core Lua types, including mappings of Lua types to Haskell.
-}
module Foreign.Lua.Api.Types where

import Data.Int
import Foreign.C
import Foreign.Ptr

#include "lua.h"

-- | An opaque structure that points to a thread and indirectly (through the
-- thread) to the whole state of a Lua interpreter. The Lua library is fully
-- reentrant: it has no global variables. All information about a state is
-- accessible through this structure.
--
-- Synonym for @lua_State *@. See <https://www.lua.org/manual/5.3/#lua_State lua_State>.
newtype LuaState = LuaState (Ptr ()) deriving (Eq)

-- | Synonym for @lua_Alloc@. See <https://www.lua.org/manual/5.3/#lua_Alloc lua_Alloc>.
type LuaAlloc = Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ())

-- | The reader function used by @'lua_load'@. Every time it needs another piece
-- of the chunk, @'lua_load'@ calls the reader, passing along its data
-- parameter. The reader must return a pointer to a block of memory with a new
-- piece of the chunk and set size to the block size. The block must exist until
-- the reader function is called again. To signal the end of the chunk, the
-- reader must return NULL or set size to zero. The reader function may return
-- pieces of any size greater than zero.
--
-- See <https://www.lua.org/manual/5.3/#lua_Reader lua_Reader>.
type LuaReader = Ptr () -> Ptr () -> Ptr CSize -> IO (Ptr CChar)

-- | Synonym for @lua_Writer@. See <https://www.lua.org/manual/5.3/#lua_Writer lua_Writer>.
type LuaWriter = LuaState -> Ptr CChar -> CSize -> Ptr () -> IO CInt

-- |  Type for C functions.
--
-- In order to communicate properly with Lua, a C function must use the
-- following protocol, which defines the way parameters and results are passed:
-- a C function receives its arguments from Lua in its stack in direct order
-- (the first argument is pushed first). So, when the function starts,
-- @'gettop'@ returns the number of arguments received by the function. The
-- first argument (if any) is at index 1 and its last argument is at index
-- @gettop@. To return values to Lua, a C function just pushes them onto the
-- stack, in direct order (the first result is pushed first), and returns the
-- number of results. Any other value in the stack below the results will be
-- properly discarded by Lua. Like a Lua function, a C function called by Lua
-- can also return many results.
--
-- See <https://www.lua.org/manual/5.3/#lua_CFunction lua_CFunction>.
type LuaCFunction = LuaState -> IO CInt

-- |  The type of integers in Lua.
--
-- By default this type is @'Int64'@, but that can be changed to different
-- values in lua. (See @LUA_INT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/#lua_Integer lua_Integer>.
type LuaInteger = #{type LUA_INTEGER}

-- |  The type of floats in Lua.
--
-- By default this type is @'Double'@, but that can be changed in Lua to a
-- single float or a long double. (See @LUA_FLOAT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/#lua_Number lua_Number>.
type LuaNumber = #{type LUA_NUMBER}

-- | Enumeration used as type tag.
-- See <https://www.lua.org/manual/5.3/#lua_type lua_type>.
data LTYPE
  = TNONE           -- ^ non-valid stack index
  | TNIL            -- ^ type of lua's @nil@ value
  | TBOOLEAN        -- ^ type of lua booleans
  | TLIGHTUSERDATA  -- ^ type of light userdata
  | TNUMBER         -- ^ type of lua numbers. See @'LuaNumber'@
  | TSTRING         -- ^ type of lua string values
  | TTABLE          -- ^ type of lua tables
  | TFUNCTION       -- ^ type of functions, either normal or @'LuaCFunction'@
  | TUSERDATA       -- ^ type of full user data
  | TTHREAD         -- ^ type of lua threads
  deriving (Bounded, Eq, Ord, Show)

instance Enum LTYPE where
  fromEnum TNONE          = #{const LUA_TNONE}
  fromEnum TNIL           = #{const LUA_TNIL}
  fromEnum TBOOLEAN       = #{const LUA_TBOOLEAN}
  fromEnum TLIGHTUSERDATA = #{const LUA_TLIGHTUSERDATA}
  fromEnum TNUMBER        = #{const LUA_TNUMBER}
  fromEnum TSTRING        = #{const LUA_TSTRING}
  fromEnum TTABLE         = #{const LUA_TTABLE}
  fromEnum TFUNCTION      = #{const LUA_TFUNCTION}
  fromEnum TUSERDATA      = #{const LUA_TUSERDATA}
  fromEnum TTHREAD        = #{const LUA_TTHREAD}

  toEnum (#{const LUA_TNONE})          = TNONE
  toEnum (#{const LUA_TNIL})           = TNIL
  toEnum (#{const LUA_TBOOLEAN})       = TBOOLEAN
  toEnum (#{const LUA_TLIGHTUSERDATA}) = TLIGHTUSERDATA
  toEnum (#{const LUA_TNUMBER})        = TNUMBER
  toEnum (#{const LUA_TSTRING})        = TSTRING
  toEnum (#{const LUA_TTABLE})         = TTABLE
  toEnum (#{const LUA_TFUNCTION})      = TFUNCTION
  toEnum (#{const LUA_TUSERDATA})      = TUSERDATA
  toEnum (#{const LUA_TTHREAD})        = TTHREAD
  toEnum n                             = error $ "Cannot convert (" ++ show n ++ ") to LTYPE"

-- | Convert number to lua type.
toLuaType :: CInt -> LTYPE
toLuaType = toEnum . fromIntegral

-- | Convert Lua type to its C representation.
fromLuaType :: LTYPE -> CInt
fromLuaType = fromIntegral . fromEnum

-- | Lua comparison operations.
data LuaRelation
  = LuaEQ -- ^ Correponds to lua's equality (==) operator.
  | LuaLT -- ^ Correponds to lua's strictly-lesser-than (<) operator
  | LuaLE -- ^ Correponds to lua's lesser-or-equal (<=) operator
  deriving (Eq, Ord, Show)

-- | Convert relation operator to its C representation.
fromLuaRelation :: LuaRelation -> CInt
#if LUA_VERSION_NUMBER >= 502
fromLuaRelation LuaEQ = #{const LUA_OPEQ}
fromLuaRelation LuaLT = #{const LUA_OPLT}
fromLuaRelation LuaLE = #{const LUA_OPLE}
#else
fromLuaRelation LuaEQ = 0
fromLuaRelation LuaLT = 1
fromLuaRelation LuaLE = 2
#endif

-- | Lua status values.
data LuaStatus
  = LuaOK        -- ^ success
  | LuaYield     -- ^ yielding / suspended coroutine
  | LuaErrRun    -- ^ a runtime rror
  | LuaErrSyntax -- ^ syntax error during precompilation
  | LuaErrMem    -- ^ memory allocation (out-of-memory) error.
  | LuaErrErr    -- ^ error while running the message handler.
  | LuaErrGcmm   -- ^ error while running a @__gc@ metamethod.
  deriving (Eq, Show)

-- | Convert C integer constant to @'LuaStatus'@.
toLuaStatus :: CInt -> LuaStatus
-- LUA_OK is not defined in Lua 5.1
toLuaStatus 0                        = LuaOK
toLuaStatus (#{const LUA_YIELD})     = LuaYield
toLuaStatus (#{const LUA_ERRRUN})    = LuaErrRun
toLuaStatus (#{const LUA_ERRSYNTAX}) = LuaErrSyntax
toLuaStatus (#{const LUA_ERRMEM})    = LuaErrMem
-- LUA_ERRGCMM did not exist in Lua 5.1; comes before LUA_ERRERR when defined
#if LUA_VERSION_NUMBER >= 502
toLuaStatus (#{const LUA_ERRGCMM})   = LuaErrGcmm
toLuaStatus (#{const LUA_ERRERR})    = LuaErrErr
#else
toLuaStatus (#{const LUA_ERRERR})    = LuaErrErr
#endif
toLuaStatus n = error $ "Cannot convert (" ++ show n ++ ") to LuaStatus"

-- | Enumeration used by @gc@ function.
data GCCONTROL
  = GCSTOP
  | GCRESTART
  | GCCOLLECT
  | GCCOUNT
  | GCCOUNTB
  | GCSTEP
  | GCSETPAUSE
  | GCSETSTEPMUL
  deriving (Enum, Eq, Ord, Show)

-- | A stack index
newtype StackIndex = StackIndex { fromStackIndex :: CInt }
  deriving (Enum, Eq, Num, Ord, Show)

--
-- Number of arguments and return values
--

-- | The number of arguments expected a function.
newtype NumArgs = NumArgs { fromNumArgs :: CInt }
  deriving (Eq, Num, Ord, Show)

-- | The number of results returned by a function call.
newtype NumResults = NumResults { fromNumResults :: CInt }
  deriving (Eq, Num, Ord, Show)
