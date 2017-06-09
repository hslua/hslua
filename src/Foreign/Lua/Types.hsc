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
{-|
Module      : Foreign.Lua.Types
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, GeneralizedNewtypeDeriving

Mapping of Lua types to Haskell.
-}
module Foreign.Lua.Types (
    GCCONTROL
  , LTYPE (..)
  , LuaState (..)
  , Lua (..)
  -- Function type synonymes
  , LuaAlloc
  , LuaCFunction
  , LuaReader
  , LuaWriter
  -- Numbers
  , LuaInteger
  , LuaNumber
  -- Stack values
  , StackIndex (..)
  -- Number of arguments/results
  , NumArgs (..)
  , NumResults (..)
  , multret
#if LUA_VERSION_NUMBER >= 502
  , LuaComparerOp (..)
#endif
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO)
import Data.Int
import Foreign.C
import Foreign.Ptr

#include "lua.h"

-- | Synonym for @lua_State *@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_State lua_State>.
newtype LuaState = LuaState (Ptr ())

-- | Lua computation
newtype Lua a = Lua { unLua :: ReaderT LuaState IO a }
  deriving (Functor, Applicative, Monad, MonadReader LuaState, MonadIO)

-- | Synonym for @lua_Alloc@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_Alloc lua_Alloc>.
type LuaAlloc = Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ())

-- | Synonym for @lua_Reader@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_Reader lua_Reader>.
type LuaReader = Ptr () -> Ptr () -> Ptr CSize -> IO (Ptr CChar)

-- | Synonym for @lua_Writer@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_Writer lua_Writer>.
type LuaWriter = LuaState -> Ptr CChar -> CSize -> Ptr () -> IO CInt

-- | Synonym for @lua_CFunction@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_CFunction lua_CFunction>.
type LuaCFunction = LuaState -> IO CInt

-- | Synonym for @lua_Integer@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_Integer lua_Integer>.
type LuaInteger = #{type LUA_INTEGER}

-- | Synonym for @lua_Number@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_Number lua_Number>.
type LuaNumber = #{type LUA_NUMBER}

-- | Enumeration used as type tag. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_type lua_type>.
data LTYPE
  = TNONE
  | TNIL
  | TBOOLEAN
  | TLIGHTUSERDATA
  | TNUMBER
  | TSTRING
  | TTABLE
  | TFUNCTION
  | TUSERDATA
  | TTHREAD
  deriving (Eq,Show,Ord)

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

#if LUA_VERSION_NUMBER >= 502
-- | Lua comparison operations
data LuaComparerOp
  = OpEQ
  | OpLT
  | OpLE
  deriving (Eq, Ord, Show)

instance Enum LuaComparerOp where
  fromEnum OpEQ = #{const LUA_OPEQ}
  fromEnum OpLT = #{const LUA_OPLT}
  fromEnum OpLE = #{const LUA_OPLE}

  toEnum (#{const LUA_OPEQ}) = OpEQ
  toEnum (#{const LUA_OPLT}) = OpLT
  toEnum (#{const LUA_OPLE}) = OpLE
  toEnum n = error $ "Cannot convert (" ++ show n ++ ") to LuaComparerOp"
#endif

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
  deriving (Eq,Ord,Show,Enum)

-- | A stack index
newtype StackIndex = StackIndex { fromStackIndex :: CInt }
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

--
-- Number of arguments and return values
--

-- | The number of arguments expected a function.
newtype NumArgs = NumArgs { fromNumArgs :: CInt }
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

-- | The number of results returned by a function call.
newtype NumResults = NumResults { fromNumResults :: CInt }
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Alias for C constant @LUA_MULTRET@. See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_call lua_call>.
multret :: NumResults
multret = NumResults $ #{const LUA_MULTRET}
