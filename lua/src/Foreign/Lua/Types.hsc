{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Foreign.Lua.Types
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

The core Lua types, including mappings of Lua types to Haskell.
-}
module Foreign.Lua.Types
  ( State (..)
  , Reader
  , GCCONTROL (..)
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , CFunction
  , LuaBool (..)
  , false
  , true
  , fromLuaBool
  , toLuaBool
  , Integer (..)
  , Number (..)
  , StackIndex (..)
  , NumArgs (..)
  , NumResults (..)
  , RelationalOperator (..)
  , fromRelationalOperator
  , Status (..)
  , StatusCode (..)
  , toStatus
  ) where

#include "lua.h"
-- required only for LUA_ERRFILE
#include "lauxlib.h"

import Prelude hiding (Integer, EQ, LT)

import Data.Int (#{type LUA_INTEGER})
import Foreign.C (CChar, CInt, CSize)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- | An opaque structure that points to a thread and indirectly (through the
-- thread) to the whole state of a Lua interpreter. The Lua library is fully
-- reentrant: it has no global variables. All information about a state is
-- accessible through this structure.
--
-- Synonym for @lua_State *@. See <https://www.lua.org/manual/5.3/#lua_State lua_State>.
newtype State = State (Ptr ()) deriving (Eq, Generic)

-- |  Type for C functions.
--
-- In order to communicate properly with Lua, a C function must use the
-- following protocol, which defines the way parameters and results are
-- passed: a C function receives its arguments from Lua in its stack in
-- direct order (the first argument is pushed first). So, when the
-- function starts, @'Foreign.Lua.Core.Functions.gettop'@ returns the
-- number of arguments received by the function. The first argument (if
-- any) is at index 1 and its last argument is at index
-- @'Foreign.Lua.Core.Functions.gettop'@. To return values to Lua, a C
-- function just pushes them onto the stack, in direct order (the first
-- result is pushed first), and returns the number of results. Any other
-- value in the stack below the results will be properly discarded by
-- Lua. Like a Lua function, a C function called by Lua can also return
-- many results.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_CFunction lua_CFunction>.
type CFunction = FunPtr (State -> IO NumResults)

-- | The reader function used by @'Foreign.Lua.Core.Functions.load'@.
-- Every time it needs another piece of the chunk, lua_load calls the
-- reader, passing along its data parameter. The reader must return a
-- pointer to a block of memory with a new piece of the chunk and set
-- size to the block size. The block must exist until the reader
-- function is called again. To signal the end of the chunk, the reader
-- must return @NULL@ or set size to zero. The reader function may
-- return pieces of any size greater than zero.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Reader lua_Reader>.
type Reader = FunPtr (State -> Ptr () -> Ptr CSize -> IO (Ptr CChar))

-- |  The type of integers in Lua.
--
-- By default this type is @'Int64'@, but that can be changed to different
-- values in lua. (See @LUA_INT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Integer lua_Integer>.
newtype Integer = Integer #{type LUA_INTEGER}
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- |  The type of floats in Lua.
--
-- By default this type is @'Double'@, but that can be changed in Lua to a
-- single float or a long double. (See @LUA_FLOAT_TYPE@ in @luaconf.h@.)
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_Number lua_Number>.
newtype Number = Number #{type LUA_NUMBER}
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
  | TypeNumber         -- ^ type of lua numbers. See @'Lua.Number'@
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
  #{const LUA_TNONE}          -> TypeNone
  #{const LUA_TNIL}           -> TypeNil
  #{const LUA_TBOOLEAN}       -> TypeBoolean
  #{const LUA_TLIGHTUSERDATA} -> TypeLightUserdata
  #{const LUA_TNUMBER}        -> TypeNumber
  #{const LUA_TSTRING}        -> TypeString
  #{const LUA_TTABLE}         -> TypeTable
  #{const LUA_TFUNCTION}      -> TypeFunction
  #{const LUA_TUSERDATA}      -> TypeUserdata
  #{const LUA_TTHREAD}        -> TypeThread
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
fromRelationalOperator EQ = #{const LUA_OPEQ}
fromRelationalOperator LT = #{const LUA_OPLT}
fromRelationalOperator LE = #{const LUA_OPLE}
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

-- | Convert C integer constant to @'Status'@.
toStatus :: StatusCode -> Status
toStatus (StatusCode c) = case c of
  #{const LUA_OK}        -> OK
  #{const LUA_YIELD}     -> Yield
  #{const LUA_ERRRUN}    -> ErrRun
  #{const LUA_ERRSYNTAX} -> ErrSyntax
  #{const LUA_ERRMEM}    -> ErrMem
  #{const LUA_ERRGCMM}   -> ErrGcmm
  #{const LUA_ERRERR}    -> ErrErr
  #{const LUA_ERRFILE}   -> ErrFile
  n -> error $ "Cannot convert (" ++ show n ++ ") to Status"
{-# INLINABLE toStatus #-}

-- | Integer code used to signal the status of a thread or computation.
-- See @'Status'@.
newtype StatusCode = StatusCode CInt deriving (Eq, Storable)


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

--
-- Number of arguments and return values
--

-- | The number of arguments consumed curing a function call.
newtype NumArgs = NumArgs { fromNumArgs :: CInt }
  deriving (Eq, Num, Ord, Show)

-- | The number of results returned by a function call.
newtype NumResults = NumResults { fromNumResults :: CInt }
  deriving (Eq, Num, Ord, Show)
