{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.FunctionCalling
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call haskell functions from Lua, and vice versa.
-}
module Foreign.Lua.FunctionCalling
  ( Peekable (..)
  , LuaCallFunc (..)
  , ToHaskellFunction (..)
  , HaskellFunction
  , Pushable (..)
  , PreCFunction
  , toHaskellFunction
  , callFunc
  , freeCFunction
  , newCFunction
  , pushHaskellFunction
  , pushPreCFunction
  , registerHaskellFunction
  ) where

import Data.ByteString (ByteString)
import Foreign.C (CInt (..))
import Foreign.Lua.Core as Lua
import Foreign.Lua.Types
import Foreign.Lua.Userdata ( ensureUserdataMetatable, pushAnyWithMetatable
                            , toAnyWithName )
import Foreign.Lua.Util (getglobal', popValue, raiseError)
import Foreign.Ptr (freeHaskellFunPtr)

-- | Type of raw Haskell functions that can be made into 'CFunction's.
type PreCFunction = Lua.State -> IO NumResults

-- | Haskell function that can be called from Lua.
type HaskellFunction = Lua NumResults

-- | Operations and functions that can be pushed to the Lua stack. This is a
-- helper function not intended to be used directly. Use the
-- @'toHaskellFunction'@ wrapper instead.
class ToHaskellFunction a where
  -- | Helper function, called by @'toHaskellFunction'@
  toHsFun :: StackIndex -> a -> Lua NumResults

instance {-# OVERLAPPING #-} ToHaskellFunction HaskellFunction where
  toHsFun _ = id

instance Pushable a => ToHaskellFunction (Lua a) where
  toHsFun _narg x = 1 <$ (x >>= push)

instance (Peekable a, ToHaskellFunction b) =>
         ToHaskellFunction (a -> b) where
  toHsFun narg f = getArg >>= toHsFun (narg + 1) . f
     where
      getArg = Lua.withExceptionMessage (errorPrefix <>) (peek narg)
      errorPrefix = "could not read argument " <>
                    show (fromStackIndex narg) <> ": "

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
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
-- E.g., the following code could be used to handle an Exception of type
-- FooException, if that type is an instance of @'MonadCatch'@ and
-- @'Pushable'@:
--
-- > toHaskellFunction (myFun `catchM` (\e -> raiseError (e :: FooException)))
--
toHaskellFunction :: ToHaskellFunction a => a -> HaskellFunction
toHaskellFunction a = toHsFun 1 a `catchException` \(Lua.Exception msg) ->
  raiseError ("Error during function call: " <> msg)

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newCFunction :: ToHaskellFunction a => a -> Lua CFunction
newCFunction = liftIO . mkWrapper . flip runWith . toHaskellFunction

-- | Turn a @'PreCFunction'@ into an actual @'CFunction'@.
foreign import ccall "wrapper"
  mkWrapper :: PreCFunction -> IO CFunction

-- | Free function pointer created with @newcfunction@.
freeCFunction :: CFunction -> Lua ()
freeCFunction = liftIO . freeHaskellFunPtr

-- | Helper class used to make lua functions useable from haskell
class LuaCallFunc a where
  callFunc' :: String -> Lua () -> NumArgs -> a

instance Peekable a => LuaCallFunc (Lua a) where
  callFunc' fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1
    popValue

instance (Pushable a, LuaCallFunc b) => LuaCallFunc (a -> b) where
  callFunc' fnName pushArgs nargs x =
    callFunc' fnName (pushArgs *> push x) (nargs + 1)

-- | Call a Lua function. Use as:
--
-- > v <- callfunc "proc" "abc" (1::Int) (5.0::Double)
callFunc :: (LuaCallFunc a) => String -> a
callFunc f = callFunc' f (return ()) 0

-- | Imports a Haskell function and registers it at global name.
registerHaskellFunction :: ToHaskellFunction a => String -> a -> Lua ()
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
pushHaskellFunction :: ToHaskellFunction a => a -> Lua ()
pushHaskellFunction hsFn = do
  pushPreCFunction . flip runWith $ toHaskellFunction hsFn
  -- Convert userdata object into a CFuntion.
  pushcclosure hslua_call_hs_ptr 1

-- | Convert callable userdata at top of stack into a CFunction, translating
-- errors to Lua errors.  Use with @'pushcclosure'@.
foreign import ccall "error-conversion.h &hslua_call_hs"
  hslua_call_hs_ptr :: CFunction

hsLuaFunctionName :: String
hsLuaFunctionName = "HsLuaFunction"

-- | Converts a pre C function to a Lua function and pushes it to the stack.
--
-- Pre C functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left in the stack.
pushPreCFunction :: PreCFunction -> Lua ()
pushPreCFunction f =
  let pushMetatable = ensureUserdataMetatable hsLuaFunctionName $ do
        -- ensure the userdata will be callable
        pushcfunction hslua_call_wrapped_hs_fun_ptr
        setfield (-2) "__call"
  in pushAnyWithMetatable pushMetatable f

-- | Call the Haskell function stored in the userdata. This function is exported
-- as a C function and then re-imported in order to get a C function pointer.
hslua_call_wrapped_hs_fun :: Lua.State -> IO NumResults
hslua_call_wrapped_hs_fun l = do
  mbFn <- runWith l (toAnyWithName stackBottom hsLuaFunctionName
                     <* remove stackBottom)
  case mbFn of
    Nothing -> runWith l (raiseError ("Could not call function" :: ByteString))
    Just fn -> fn l

foreign export ccall hslua_call_wrapped_hs_fun :: PreCFunction
foreign import ccall "&hslua_call_wrapped_hs_fun"
  hslua_call_wrapped_hs_fun_ptr :: CFunction
