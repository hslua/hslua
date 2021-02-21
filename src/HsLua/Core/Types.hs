{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    -- * References
  , Reference (..)
  , fromReference
  , toReference
    -- * Stack index helpers
  , nthTop
  , nthBottom
  , nth
  , top
  ) where

import Prelude hiding (Integer)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO, asks, liftIO)
import Foreign.Lua (nth, nthBottom, nthTop, top)
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
