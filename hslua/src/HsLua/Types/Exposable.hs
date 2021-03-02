{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Types.Exposable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call haskell functions from Lua, and vice versa.
-}
module HsLua.Types.Exposable
  ( Exposable (..)
  , HaskellFunction
  , Pushable (..)
  , PreCFunction
  , toHaskellFunction
  , pushHaskellFunction
  , pushPreCFunction
  , registerHaskellFunction
  ) where

import HsLua.Core as Lua
import HsLua.Core.Types (liftLua)
import Lua.Call (hslua_pushhsfunction)
import HsLua.Types.Peekable (Peekable (peek))
import HsLua.Types.Pushable (Pushable (push))

-- | Type of raw Haskell functions that can be made into
-- 'CFunction's.
type PreCFunction = State -> IO NumResults

-- | Haskell function that can be called from Lua.
type HaskellFunction = Lua NumResults

-- | Operations and functions that can be pushed to the Lua stack. This
-- is a helper function not intended to be used directly. Use the
-- @'toHaskellFunction'@ wrapper instead.
class Exposable a where
  -- | Helper function, called by @'toHaskellFunction'@. Should do a
  -- partial application of the argument at the given index to the
  -- underlying function. Recurses if necessary, causing further partial
  -- applications until the operation is a easily exposable to Lua.
  partialApply :: StackIndex -> a -> Lua NumResults

instance {-# OVERLAPPING #-} Exposable HaskellFunction where
  partialApply _ = id

instance Pushable a => Exposable (Lua a) where
  partialApply _narg x = 1 <$ (x >>= push)

instance (Peekable a, Exposable b) => Exposable (a -> b) where
  partialApply narg f = getArg >>= partialApply (narg + 1) . f
    where
      getArg = Lua.withExceptionMessage (errorPrefix <>) (peek narg)
      errorPrefix = "could not read argument " <>
                    show (fromStackIndex narg) <> ": "

-- | Convert a Haskell function to a function type directly exposable to
-- Lua. Any Haskell function can be converted provided that:
--
--   * all arguments are instances of @'Peekable'@
--   * return type is @Lua a@, where @a@ is an instance of
--     @'Pushable'@
--
-- Any @'Lua.Exception'@ will be converted to a string and returned
-- as Lua error.
--
-- /Important/: this does __not__ catch exceptions other than
-- @'Lua.Exception'@; exception handling must be done by the converted
-- Haskell function. Failure to do so will cause the program to crash.
--
-- E.g., the following code could be used to handle an Exception
-- of type FooException, if that type is an instance of
-- 'Control.Monad.Catch.MonadCatch' and 'Pushable':
--
-- > toHaskellFunction (myFun `catchM` (\e -> raiseError (e :: FooException)))
--
toHaskellFunction :: Exposable a => a -> HaskellFunction
toHaskellFunction a = do
  errConv <- Lua.errorConversion
  let ctx = "Error during function call: "
  Lua.exceptionToError errConv . Lua.addContextToException errConv ctx $
    partialApply 1 a

-- | Imports a Haskell function and registers it at global name.
registerHaskellFunction :: Exposable a => String -> a -> Lua ()
registerHaskellFunction n f = do
  pushHaskellFunction f
  setglobal n

-- | Pushes Haskell function as a callable userdata.
-- All values created will be garbage collected. Use as:
--
-- > pushHaskellFunction myfun
-- > setglobal "myfun"
--
-- Error conditions should be indicated by raising a Lua @'Lua.Exception'@
-- or by returning the result of @'Lua.error'@.
pushHaskellFunction :: Exposable a => a -> Lua ()
pushHaskellFunction hsFn = do
  errConv <- Lua.errorConversion
  preCFn <- return . flip (runWithConverter errConv) $ toHaskellFunction hsFn
  pushPreCFunction preCFn

-- | Converts a pre C function to a Lua function and pushes it to the stack.
--
-- Pre C functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left on the stack.
pushPreCFunction :: PreCFunction -> Lua ()
pushPreCFunction preCFn = liftLua $ \l ->
  hslua_pushhsfunction l preCFn
