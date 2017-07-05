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
{-# LANGUAGE DeriveDataTypeable         #-}
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
module Foreign.Lua.Types.Core (
    GCCONTROL
  , LTYPE (..)
  , LuaState (..)
  , Lua (..)
  , LuaException (..)
  , runLuaWith
  , luaState
  , liftIO
  , liftLua
  , liftLua1
  , catchLuaError
  , throwLuaError
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
  , LuaComparerOp (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, catch)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO, ask, liftIO)
import Data.Int
import Data.Typeable (Typeable)
import Foreign.C
import Foreign.Ptr

#include "lua.h"

-- | Synonym for @lua_State *@. See <https://www.lua.org/manual/5.3/#lua_State lua_State>.
newtype LuaState = LuaState (Ptr ())

-- | Lua computation
newtype Lua a = Lua { unLua :: ReaderT LuaState IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadReader LuaState
    , MonadThrow
    )

data LuaException = LuaException String
  deriving (Typeable)

instance Show LuaException where
  show (LuaException err) = err

instance Exception LuaException

throwLuaError :: String -> Lua a
throwLuaError = throwM . LuaException

catchLuaError :: Lua a -> (LuaException -> Lua a) -> Lua a
catchLuaError = catch

-- | Turn a function of typ @LuaState -> IO a@ into a monadic lua operation.
liftLua :: (LuaState -> IO a) -> Lua a
liftLua f = luaState >>= liftIO . f

-- | Turn a function of typ @LuaState -> a -> IO b@ into a monadic lua operation.
liftLua1 :: (LuaState -> a -> IO b) -> a -> Lua b
liftLua1 f x = liftLua $ \l -> f l x

-- | Get the lua state of this lua computation.
luaState :: Lua LuaState
luaState = ask

-- | Run lua computation with custom lua state.
runLuaWith :: LuaState -> Lua a -> IO a
runLuaWith l s = runReaderT (unLua s) l
  `catch` \ (LuaException err) -> error err

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

-- | Synonym for @lua_Integer@. See <https://www.lua.org/manual/5.3/#lua_Integer lua_Integer>.
type LuaInteger = #{type LUA_INTEGER}

-- | Synonym for @lua_Number@. See <https://www.lua.org/manual/5.3/#lua_Number lua_Number>.
type LuaNumber = #{type LUA_NUMBER}

-- | Enumeration used as type tag. See <https://www.lua.org/manual/5.3/#lua_type lua_type>.
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

-- | Lua comparison operations
data LuaComparerOp
  = OpEQ -- ^ Correponds to lua's equality (==) operator
  | OpLT -- ^ Correponds to lua's strictly-lesser-than (<) operator
  | OpLE -- ^ Correponds to lua's lesser-or-equal (<=) operator
  deriving (Eq, Ord, Show)

#if LUA_VERSION_NUMBER >= 502
instance Enum LuaComparerOp where
  fromEnum OpEQ = #{const LUA_OPEQ}
  fromEnum OpLT = #{const LUA_OPLT}
  fromEnum OpLE = #{const LUA_OPLE}

  toEnum (#{const LUA_OPEQ}) = OpEQ
  toEnum (#{const LUA_OPLT}) = OpLT
  toEnum (#{const LUA_OPLE}) = OpLE
  toEnum n = error $ "Cannot convert (" ++ show n ++ ") to LuaComparerOp"
#else
deriving instance Enum LuaComparerOp
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
  deriving (Enum, Eq, Ord, Show)

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
