{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-|
Module      : HsLua.Core.Types
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

The core Lua types, including mappings of Lua types to Haskell.

This module has mostly been moved to @'Foreign.Lua.Types'@ and
currently re-exports that module. This module might be removed in
the future.
-}
module HsLua.Core.Types
  ( Lua (..)
  , LuaEnvironment (..)
  , ErrorConversion (..)
  , errorConversion
  , State (..)
  , Reader
  , liftLua
  , liftLua1
  , state
  , runWithConverter
  , unsafeRunWith
  , unsafeErrorConversion
  , GCCONTROL (..)
  , toGCCode
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , liftIO
  , CFunction
  , LuaBool (..)
  , fromLuaBool
  , toLuaBool
  , Integer (..)
  , Number (..)
  , StackIndex (..)
  , registryindex
  , NumArgs (..)
  , NumResults (..)
  , multret
  , RelationalOperator (..)
  , fromRelationalOperator
  , Status (..)
  , StatusCode (..)
  , toStatus
    -- * References
  , Reference (..)
  , fromReference
  , toReference
  , noref
  , refnil
    -- * Stack index helpers
  , nthTop
  , nthBottom
  , nth
  , top
  ) where

import Prelude hiding (Integer, EQ, LT)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO, asks, liftIO)
import Foreign.Lua (nth, nthBottom, nthTop, top)
import Foreign.Lua.Constants
import Foreign.Lua.Types
import Foreign.Lua.Auxiliary
  ( Reference (..)
  , fromReference
  , toReference
  )

-- | Define the ways in which exceptions and errors are handled.
data ErrorConversion = ErrorConversion
  { errorToException :: forall a . State -> IO a
    -- ^ Translate Lua errors to Haskell exceptions
  , addContextToException :: forall a . String -> Lua a -> Lua a
    -- ^ Add information on the current context to an exception.
  , alternative :: forall a . Lua a -> Lua a -> Lua a
    -- ^ Runs the second computation only if the first fails; returns
    -- the result of the first successful computation, if any.
  , exceptionToError :: Lua NumResults -> Lua NumResults
    -- ^ Translate Haskell exceptions to Lua errors
  }

-- | Environment in which Lua computations are evaluated.
data LuaEnvironment = LuaEnvironment
  { luaEnvErrorConversion :: ErrorConversion
    -- ^ Functions for error and exception handling and conversion
  , luaEnvState :: State
    -- ^ Lua interpreter state
  }

-- | A Lua computation. This is the base type used to run Lua programs of any
-- kind. The Lua state is handled automatically, but can be retrieved via
-- @'state'@.
newtype Lua a = Lua { unLua :: ReaderT LuaEnvironment IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadReader LuaEnvironment
    , MonadThrow
    )

-- | Turn a function of typ @Lua.State -> IO a@ into a monadic Lua operation.
liftLua :: (State -> IO a) -> Lua a
liftLua f = state >>= liftIO . f

-- | Turn a function of typ @Lua.State -> a -> IO b@ into a monadic Lua operation.
liftLua1 :: (State -> a -> IO b) -> a -> Lua b
liftLua1 f x = liftLua $ \l -> f l x

-- | Get the Lua state of this Lua computation.
state :: Lua State
state = asks luaEnvState

-- | Get the error-to-exception function.
errorConversion :: Lua ErrorConversion
errorConversion = asks luaEnvErrorConversion

-- | Run Lua computation with the given Lua state and error-to-exception
-- converter. Any resulting exceptions are left unhandled.
runWithConverter :: ErrorConversion -> State -> Lua a -> IO a
runWithConverter e2e l s =
  runReaderT (unLua s) (LuaEnvironment e2e l)

-- | Run the given operation, but crash if any Haskell exceptions occur.
unsafeRunWith :: State -> Lua a -> IO a
unsafeRunWith = runWithConverter unsafeErrorConversion

-- | Unsafe @'ErrorConversion'@; no proper error handling is attempted,
-- any error leads to a crash.
unsafeErrorConversion :: ErrorConversion
unsafeErrorConversion = ErrorConversion
  { errorToException = const (error "An unrecoverable Lua error occured.")
  , addContextToException = const id
  , alternative = const
  , exceptionToError = id
  }


--
-- Type of Lua values
--

-- | Enumeration used as type tag.
-- See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>.
data Type
  = TypeNone           -- ^ non-valid stack index
  | TypeNil            -- ^ type of Lua's @nil@ value
  | TypeBoolean        -- ^ type of Lua booleans
  | TypeLightUserdata  -- ^ type of light userdata
  | TypeNumber         -- ^ type of Lua numbers. See @'Lua.Number'@
  | TypeString         -- ^ type of Lua string values
  | TypeTable          -- ^ type of Lua tables
  | TypeFunction       -- ^ type of functions, either normal or @'CFunction'@
  | TypeUserdata       -- ^ type of full user data
  | TypeThread         -- ^ type of Lua threads
  deriving (Bounded, Eq, Ord, Show)

instance Enum Type where
  fromEnum = fromIntegral . fromTypeCode . fromType
  toEnum = toType . TypeCode . fromIntegral

-- | Convert a Lua 'Type' to a type code which can be passed to the C
-- API.
fromType :: Type -> TypeCode
fromType = \case
  TypeNone          -> LUA_TNONE
  TypeNil           -> LUA_TNIL
  TypeBoolean       -> LUA_TBOOLEAN
  TypeLightUserdata -> LUA_TLIGHTUSERDATA
  TypeNumber        -> LUA_TNUMBER
  TypeString        -> LUA_TSTRING
  TypeTable         -> LUA_TTABLE
  TypeFunction      -> LUA_TFUNCTION
  TypeUserdata      -> LUA_TUSERDATA
  TypeThread        -> LUA_TTHREAD

-- | Convert numerical code to Lua 'Type'.
toType :: TypeCode -> Type
toType = \case
  LUA_TNONE          -> TypeNone
  LUA_TNIL           -> TypeNil
  LUA_TBOOLEAN       -> TypeBoolean
  LUA_TLIGHTUSERDATA -> TypeLightUserdata
  LUA_TNUMBER        -> TypeNumber
  LUA_TSTRING        -> TypeString
  LUA_TTABLE         -> TypeTable
  LUA_TFUNCTION      -> TypeFunction
  LUA_TUSERDATA      -> TypeUserdata
  LUA_TTHREAD        -> TypeThread
  TypeCode c         -> error ("No Type corresponding to " ++ show c)


--
-- Thread status
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
toStatus = \case
  LUA_OK        -> OK
  LUA_YIELD     -> Yield
  LUA_ERRRUN    -> ErrRun
  LUA_ERRSYNTAX -> ErrSyntax
  LUA_ERRMEM    -> ErrMem
  LUA_ERRGCMM   -> ErrGcmm
  LUA_ERRERR    -> ErrErr
  LUA_ERRFILE   -> ErrFile
  StatusCode n  -> error $ "Cannot convert (" ++ show n ++ ") to Status"
{-# INLINABLE toStatus #-}

--
-- Relational Operator
--

-- | Lua comparison operations.
data RelationalOperator
  = EQ -- ^ Correponds to Lua's equality (==) operator.
  | LT -- ^ Correponds to Lua's strictly-lesser-than (<) operator
  | LE -- ^ Correponds to Lua's lesser-or-equal (<=) operator
  deriving (Eq, Ord, Show)

-- | Convert relation operator to its C representation.
fromRelationalOperator :: RelationalOperator -> OPCode
fromRelationalOperator = \case
  EQ -> LUA_OPEQ
  LT -> LUA_OPLT
  LE -> LUA_OPLE
{-# INLINABLE fromRelationalOperator #-}

--
-- Boolean
--

-- | Convert a @'LuaBool'@ to a Haskell @'Bool'@.
fromLuaBool :: LuaBool -> Bool
fromLuaBool FALSE = False
fromLuaBool _     = True
{-# INLINABLE fromLuaBool #-}

-- | Convert a Haskell @'Bool'@ to a @'LuaBool'@.
toLuaBool :: Bool -> LuaBool
toLuaBool True  = TRUE
toLuaBool False = FALSE
{-# INLINABLE toLuaBool #-}

--
-- Garbage collection
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
  | GCISRUNNING
  deriving (Enum, Eq, Ord, Show)

toGCCode :: GCCONTROL -> GCCode
toGCCode = \case
  GCSTOP       -> LUA_GCSTOP
  GCRESTART    -> LUA_GCRESTART
  GCCOLLECT    -> LUA_GCCOLLECT
  GCCOUNT      -> LUA_GCCOUNT
  GCCOUNTB     -> LUA_GCCOUNTB
  GCSTEP       -> LUA_GCSTEP
  GCSETPAUSE   -> LUA_GCSETPAUSE
  GCSETSTEPMUL -> LUA_GCSETSTEPMUL
  GCISRUNNING  -> LUA_GCISRUNNING

--
-- Special values
--

-- | Option for multiple returns in @'pcall'@.
multret :: NumResults
multret = LUA_MULTRET

-- | Pseudo stack index of the Lua registry.
registryindex :: StackIndex
registryindex = LUA_REGISTRYINDEX

-- | Value signaling that no reference was created.
refnil :: Int
refnil = fromIntegral LUA_REFNIL

-- | Value signaling that no reference was found.
noref :: Int
noref = fromIntegral LUA_NOREF
