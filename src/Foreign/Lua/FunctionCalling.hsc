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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.FunctionCalling
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
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
  , registerHaskellFunction
  ) where

import Control.Monad (when)
import Data.Monoid ((<>))
import Foreign.C (CInt (..))
import Foreign.Lua.Core
import Foreign.Lua.Types
import Foreign.Lua.Util (getglobal', raiseError)
import Foreign.Ptr (castPtr, freeHaskellFunPtr)
import Foreign.StablePtr (deRefStablePtr, newStablePtr)

import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.Storable as F

-- | Type of raw haskell functions that can be made into 'CFunction's.
type PreCFunction = LuaState -> IO NumResults

-- | Haskell function that can be called from Lua.
type HaskellFunction = Lua NumResults

-- | Operations and functions that can be pushed to the lua stack. This is a
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
      getArg = safePeek narg >>= \case
        Success x -> return x
        Error msg -> throwLuaError $ "could not read argument "
                     <> Char8.pack (show (fromStackIndex narg)) <> ": "
                     <> mconcat msg

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of @'Peekable'@
--   * return type is @Lua a@, where @a@ is an instance of
--     @'Pushable'@
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
toHaskellFunction :: ToHaskellFunction a => a -> HaskellFunction
toHaskellFunction a = toHsFun 1 a `catchLuaError` \err ->
  raiseError ("Error during function call: " ++ show err)

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newCFunction :: ToHaskellFunction a => a -> Lua CFunction
newCFunction = liftIO . mkWrapper . flip runLuaWith . toHaskellFunction

-- | Turn a @'PreCFunction'@ into an actual @'CFunction'@.
foreign import ccall "wrapper"
  mkWrapper :: PreCFunction -> IO CFunction

-- | Free function pointer created with @newcfunction@.
freeCFunction :: CFunction -> Lua ()
freeCFunction = liftIO . freeHaskellFunPtr

-- | Helper class used to make lua functions useable from haskell
class LuaCallFunc a where
  callFunc' :: String -> Lua () -> NumArgs -> a

instance Peekable a => LuaCallFunc (Lua (Result a)) where
  callFunc' fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1
    safePeek (-1) <* pop 1

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


-- | Convert callable userdata at top of stack into a CFunction, translating
-- errors to Lua errors.
foreign import ccall "safer-api.h &hslua_call_hs"
  hslua_call_hs_ptr :: CFunction

-- | Convert a Haskell function userdata object into a CFuntion. The userdata
-- object must be at the top of the stack. Errors signaled via @'error'@ are
-- converted to lua errors.
wrapHaskellFunction :: Lua ()
wrapHaskellFunction = do
  t <- ltype (-1)
  case t of
    TypeUserdata -> pushcclosure hslua_call_hs_ptr 1
    _ -> throwLuaError "Need HaskellImportedFunction to create a CFunction."

-- | Pushes Haskell function as a callable userdata.
-- All values created will be garbage collected. Use as:
--
-- > pushHaskellFunction myfun
-- > setglobal "myfun"
--
-- You are not allowed to use @lua_error@ anywhere, but
-- use an error code of (-1) to the same effect. Push
-- error message as the sole return value.
pushHaskellFunction :: ToHaskellFunction a => a -> Lua ()
pushHaskellFunction hsFn = do
  pushPreCFunction . flip runLuaWith $ toHaskellFunction hsFn
  wrapHaskellFunction

-- | Converts a pre C function to a Lua function and pushes it to the stack.
--
-- Pre C functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left in the stack.
pushPreCFunction :: PreCFunction -> Lua ()
pushPreCFunction f = do
  stableptr <- liftIO $ newStablePtr f
  p <- newuserdata (F.sizeOf stableptr)
  liftIO $ F.poke (castPtr p) stableptr
  v <- newmetatable "HaskellImportedFunction"
  when v $ do
    -- create new metatable, fill it with two entries __gc and __call
    pushcfunction hslua_userdata_gc_ptr
    setfield (-2) "__gc"
    pushcfunction hslua_call_wrapped_hs_fun_ptr
    setfield (-2) "__call"
  setmetatable (-2)
  return ()

-- | Function to free the stable pointer in a userdata, ensuring the Haskell
-- value can be garbage collected. This function does not call back into
-- Haskell, making is safe to call even from functions imported as unsafe.
foreign import ccall "&hslua_userdata_gc"
  hslua_userdata_gc_ptr :: CFunction

-- | Call the Haskell function stored in the userdata. This function is exported
-- as a C function and then re-imported in order to get a C function pointer.
hslua_call_wrapped_hs_fun :: LuaState -> IO NumResults
hslua_call_wrapped_hs_fun l = do
  ptr <- runLuaWith l $ peek 1 <* remove 1
  stableptr <- F.peek (castPtr ptr)
  f <- deRefStablePtr stableptr
  f l

foreign export ccall hslua_call_wrapped_hs_fun :: PreCFunction
foreign import ccall "&hslua_call_wrapped_hs_fun"
  hslua_call_wrapped_hs_fun_ptr :: CFunction
