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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-|
Module      : Foreign.Lua.Core.Types
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

The core Lua types, including mappings of Lua types to Haskell.
-}
module Foreign.Lua.Core.Types
  ( Lua (..)
  , LuaState (..)
  , LuaReader
  , liftLua
  , liftLua1
  , luaState
  , runLuaWith
  , GCCONTROL (..)
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , liftIO
  , CFunction
  , LuaBool (..)
  , false
  , true
  , fromLuaBool
  , toLuaBool
  , LuaInteger (..)
  , LuaNumber (..)
  , StackIndex (..)
  , nthFromBottom
  , nthFromTop
  , stackTop
  , stackBottom
  , NumArgs (..)
  , NumResults (..)
  , RelationalOperator (..)
  , fromRelationalOperator
  , Status (..)
  , StatusCode (..)
  , toStatus
  , Failable (..)
  ) where

import Prelude hiding (EQ, LT)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO, ask, liftIO)
import Data.Int (#{type LUA_INTEGER})
import Foreign.C (CChar, CInt, CSize)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)

#include "lua.h"
-- required only for LUA_ERRFILE
#include "lauxlib.h"

-- | A Lua computation. This is the base type used to run Lua programs of any
-- kind. The Lua state is handled automatically, but can be retrieved via
-- @'luaState'@.
newtype Lua a = Lua { unLua :: ReaderT LuaState IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadReader LuaState
    , MonadThrow
    )

-- | Turn a function of typ @LuaState -> IO a@ into a monadic lua operation.
liftLua :: (LuaState -> IO a) -> Lua a
liftLua f = luaState >>= liftIO . f

-- | Turn a function of typ @LuaState -> a -> IO b@ into a monadic lua operation.
liftLua1 :: (LuaState -> a -> IO b) -> a -> Lua b
liftLua1 f x = liftLua $ \l -> f l x

-- | Get the lua state of this lua computation.
luaState :: Lua LuaState
luaState = ask

-- | Run lua computation with custom lua state. Errors are left unhandled, the
-- caller of this function is responsible to catch lua errors.
runLuaWith :: LuaState -> Lua a -> IO a
runLuaWith l s = runReaderT (unLua s) l

-- | An opaque structure that points to a thread and indirectly (through the
-- thread) to the whole state of a Lua interpreter. The Lua library is fully
-- reentrant: it has no global variables. All information about a state is
-- accessible through this structure.
--
-- Synonym for @lua_State *@. See <https://www.lua.org/manual/5.3/#lua_State lua_State>.
newtype LuaState = LuaState (Ptr ()) deriving (Eq)

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
-- See <https://www.lua.org/manual/5.3/manual.html#lua_CFunction lua_CFunction>.
type CFunction = FunPtr (LuaState -> IO NumResults)

-- | The reader function used by @'lua_load'@. Every time it needs another piece
-- of the chunk, lua_load calls the reader, passing along its data parameter.
-- The reader must return a pointer to a block of memory with a new piece of the
-- chunk and set size to the block size. The block must exist until the reader
-- function is called again. To signal the end of the chunk, the reader must
-- return @NULL@ or set size to zero. The reader function may return pieces of any
-- size greater than zero.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Reader lua_Reader>.
type LuaReader = FunPtr (LuaState -> Ptr () -> Ptr CSize -> IO (Ptr CChar))

-- |  The type of integers in Lua.
--
-- By default this type is @'Int64'@, but that can be changed to different
-- values in lua. (See @LUA_INT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Integer lua_Integer>.
newtype LuaInteger = LuaInteger #{type LUA_INTEGER}
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

-- |  The type of floats in Lua.
--
-- By default this type is @'Double'@, but that can be changed in Lua to a
-- single float or a long double. (See @LUA_FLOAT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Number lua_Number>.
newtype LuaNumber = LuaNumber #{type LUA_NUMBER}
  deriving (Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac, Show)


--
-- LuaBool
--

-- | Boolean value returned by a Lua C API function. This is a @'CInt'@ and
-- interpreted as @'False'@ iff the value is @0@, @'True'@ otherwise.
newtype LuaBool = LuaBool CInt
  deriving (Eq, Storable, Show)

-- | Generic Lua representation of a value interpreted as being true.
true :: LuaBool
true = LuaBool 1

-- | Lua representation of the value interpreted as false.
false :: LuaBool
false = LuaBool 0

-- | Convert a @'LuaBool'@ to a Haskell @'Bool'@.
fromLuaBool :: LuaBool -> Bool
fromLuaBool (LuaBool 0) = False
fromLuaBool _           = True
{-# INLINABLE fromLuaBool #-}

-- | Convert a Haskell @'Bool'@ to a @'LuaBool'@.
toLuaBool :: Bool -> LuaBool
toLuaBool True  = true
toLuaBool False = false
{-# INLINABLE toLuaBool #-}


--
-- * Type of Lua values
--

-- | Enumeration used as type tag.
-- See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>.
data Type
  = TypeNone           -- ^ non-valid stack index
  | TypeNil            -- ^ type of lua's @nil@ value
  | TypeBoolean        -- ^ type of lua booleans
  | TypeLightUserdata  -- ^ type of light userdata
  | TypeNumber         -- ^ type of lua numbers. See @'LuaNumber'@
  | TypeString         -- ^ type of lua string values
  | TypeTable          -- ^ type of lua tables
  | TypeFunction       -- ^ type of functions, either normal or @'CFunction'@
  | TypeUserdata       -- ^ type of full user data
  | TypeThread         -- ^ type of lua threads
  deriving (Bounded, Eq, Ord, Show)

-- | Integer code used to encode the type of a lua value.
newtype TypeCode = TypeCode { fromTypeCode :: CInt }
  deriving (Eq, Ord, Show)

instance Enum Type where
  fromEnum = fromIntegral . fromTypeCode . fromType
  toEnum = toType . TypeCode . fromIntegral

-- | Convert a lua Type to a type code which can be passed to the C API.
fromType :: Type -> TypeCode
fromType tp = TypeCode $ case tp of
  TypeNone          -> #{const LUA_TNONE}
  TypeNil           -> #{const LUA_TNIL}
  TypeBoolean       -> #{const LUA_TBOOLEAN}
  TypeLightUserdata -> #{const LUA_TLIGHTUSERDATA}
  TypeNumber        -> #{const LUA_TNUMBER}
  TypeString        -> #{const LUA_TSTRING}
  TypeTable         -> #{const LUA_TTABLE}
  TypeFunction      -> #{const LUA_TFUNCTION}
  TypeUserdata      -> #{const LUA_TUSERDATA}
  TypeThread        -> #{const LUA_TTHREAD}

-- | Convert numerical code to lua type.
toType :: TypeCode -> Type
toType (TypeCode c) = case c of
  (#{const LUA_TNONE})          -> TypeNone
  (#{const LUA_TNIL})           -> TypeNil
  (#{const LUA_TBOOLEAN})       -> TypeBoolean
  (#{const LUA_TLIGHTUSERDATA}) -> TypeLightUserdata
  (#{const LUA_TNUMBER})        -> TypeNumber
  (#{const LUA_TSTRING})        -> TypeString
  (#{const LUA_TTABLE})         -> TypeTable
  (#{const LUA_TFUNCTION})      -> TypeFunction
  (#{const LUA_TUSERDATA})      -> TypeUserdata
  (#{const LUA_TTHREAD})        -> TypeThread
  _ -> error ("No Type corresponding to " ++ show c)

--
-- * Relational Operator
--

-- | Lua comparison operations.
data RelationalOperator
  = EQ -- ^ Correponds to lua's equality (==) operator.
  | LT -- ^ Correponds to lua's strictly-lesser-than (<) operator
  | LE -- ^ Correponds to lua's lesser-or-equal (<=) operator
  deriving (Eq, Ord, Show)

-- | Convert relation operator to its C representation.
fromRelationalOperator :: RelationalOperator -> CInt
#if LUA_VERSION_NUMBER >= 502
fromRelationalOperator EQ = #{const LUA_OPEQ}
fromRelationalOperator LT = #{const LUA_OPLT}
fromRelationalOperator LE = #{const LUA_OPLE}
#else
fromRelationalOperator EQ = 0
fromRelationalOperator LT = 1
fromRelationalOperator LE = 2
#endif
{-# INLINABLE fromRelationalOperator #-}


--
-- * Status
--

-- | Lua status values.
data Status
  = OK        -- ^ success
  | Yield     -- ^ yielding / suspended coroutine
  | ErrRun    -- ^ a runtime rror
  | ErrSyntax -- ^ syntax error during precompilation
  | ErrMem    -- ^ memory allocation (out-of-memory) error.
  | ErrErr    -- ^ error while running the message handler.
  | ErrGcmm   -- ^ error while running a @__gc@ metamethod.
  | ErrFile   -- ^ opening or reading a file failed.
  deriving (Eq, Show)

-- | Convert C integer constant to @'LuaStatus'@.
toStatus :: StatusCode -> Status
toStatus (StatusCode c) = case c of
  -- LUA_OK is not defined in Lua 5.1
  0                        -> OK
  (#{const LUA_YIELD})     -> Yield
  (#{const LUA_ERRRUN})    -> ErrRun
  (#{const LUA_ERRSYNTAX}) -> ErrSyntax
  (#{const LUA_ERRMEM})    -> ErrMem
  -- LUA_ERRGCMM did not exist in Lua 5.1; comes before LUA_ERRERR when defined
#if LUA_VERSION_NUMBER >= 502
  (#{const LUA_ERRGCMM})   -> ErrGcmm
  (#{const LUA_ERRERR})    -> ErrErr
#else
  (#{const LUA_ERRERR})    -> ErrErr
#endif
  (#{const LUA_ERRFILE})   -> ErrFile
  n -> error $ "Cannot convert (" ++ show n ++ ") to LuaStatus"
{-# INLINABLE toStatus #-}

-- | Integer code used to signal the status of a thread or computation.
-- See @'Status'@.
newtype StatusCode = StatusCode CInt deriving Eq

-- | CInt value or an error, using the convention that value below zero indicate
-- an error. Values greater than zero are used verbatim. The phantom type is
-- used for additional type safety and gives the type into which the wrapped
-- CInt should be converted.
newtype Failable a = Failable CInt


--
-- * Gargabe Collection Control
--

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

-- | Stack index of the nth element from the top of the stack.
nthFromTop :: CInt -> StackIndex
nthFromTop n = StackIndex (-n)
{-# INLINABLE nthFromTop #-}

-- | Stack index of the nth element from the bottom of the stack.
nthFromBottom :: CInt -> StackIndex
nthFromBottom = StackIndex
{-# INLINABLE nthFromBottom #-}

-- | Top of the stack
stackTop :: StackIndex
stackTop = -1
{-# INLINABLE stackTop #-}

-- | Bottom of the stack
stackBottom :: StackIndex
stackBottom = 1
{-# INLINABLE stackBottom #-}

--
-- Number of arguments and return values
--

-- | The number of arguments expected a function.
newtype NumArgs = NumArgs { fromNumArgs :: CInt }
  deriving (Eq, Num, Ord, Show)

-- | The number of results returned by a function call.
newtype NumResults = NumResults { fromNumResults :: CInt }
  deriving (Eq, Num, Ord, Show)
