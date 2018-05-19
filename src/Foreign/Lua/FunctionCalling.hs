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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
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
  ( FromLuaStack (..)
  , LuaCallFunc (..)
  , ToHaskellFunction (..)
  , HaskellFunction
  , ToLuaStack (..)
  , PreCFunction
  , toHaskellFunction
  , callFunc
  , freeCFunction
  , newCFunction
  , pushHaskellFunction
  , registerHaskellFunction
  ) where

import Control.Monad (when)
import Foreign.C (CInt (..))
import Foreign.Lua.Core
import Foreign.Lua.Types
import Foreign.Lua.Util (getglobal')
import Foreign.Ptr (castPtr, freeHaskellFunPtr)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)

import qualified Data.ByteString.Char8 as BS
import qualified Foreign.Lua.Core as Lua
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

instance ToLuaStack a => ToHaskellFunction (Lua a) where
  toHsFun _narg x = 1 <$ (x >>= push)

instance (FromLuaStack a, ToHaskellFunction b) =>
         ToHaskellFunction (a -> b) where
  toHsFun narg f = getArg >>= toHsFun (narg + 1) . f
     where
      getArg = safePeek narg >>= \case
        Success x -> return x
        Error msg -> throwLuaError $ "could not read argument "
                     ++ show (fromStackIndex narg) ++ ": "
                     ++ mconcat (map BS.unpack msg)

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of @'FromLuaStack'@
--   * return type is @Lua a@, where @a@ is an instance of
--     @'ToLuaStack'@
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
toHaskellFunction :: ToHaskellFunction a => a -> HaskellFunction
toHaskellFunction a = toHsFun 1 a `catchLuaError` \err -> do
  push ("Error during function call: " ++ show err)
  fromIntegral <$> Lua.error

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

instance (FromLuaStack a) => LuaCallFunc (Lua (Result a)) where
  callFunc' fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1
    safePeek (-1) <* pop 1

instance (ToLuaStack a, LuaCallFunc b) => LuaCallFunc (a -> b) where
  callFunc' fnName pushArgs nargs x =
    callFunc' fnName (pushArgs *> push x) (nargs + 1)

-- | Call a Lua function. Use as:
--
-- > v <- callfunc "proc" "abc" (1::Int) (5.0::Double)
callFunc :: (LuaCallFunc a) => String -> a
callFunc f = callFunc' f (return ()) 0

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
pushHaskellFunction = pushPreCFunction . flip runLuaWith . toHaskellFunction

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
    pushcfunction hsmethod__gc_addr
    setfield (-2) "__gc"
    pushcfunction hsmethod__call_addr
    setfield (-2) "__call"
  setmetatable (-2)
  return ()

-- | Imports a Haskell function and registers it at global name.
registerHaskellFunction :: ToHaskellFunction a => String -> a -> Lua ()
registerHaskellFunction n f = do
  pushHaskellFunction f
  setglobal n

foreign export ccall hsMethodGc :: PreCFunction
foreign import ccall "&hsMethodGc" hsmethod__gc_addr :: CFunction

foreign export ccall hsMethodCall :: PreCFunction
foreign import ccall "&hsMethodCall" hsmethod__call_addr :: CFunction

hsMethodGc :: LuaState -> IO NumResults
hsMethodGc l = do
  ptr <- runLuaWith l $ peek (-1)
  stableptr <- F.peek (castPtr ptr)
  freeStablePtr stableptr
  return 0

hsMethodCall :: LuaState -> IO NumResults
hsMethodCall l = do
  ptr <- runLuaWith l $ peek 1 <* remove 1
  stableptr <- F.peek (castPtr ptr)
  f <- deRefStablePtr stableptr
  f l
