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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.FunctionCalling
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call haskell functions from Lua, and vice versa.
-}
module Foreign.Lua.FunctionCalling
  ( FromLuaStack (..)
  , LuaCallFunc (..)
  , LuaImport (..)
  , ToLuaStack (..)
  , PreCFunction
  , freecfunction
  , luaimport
  , callfunc
  , newcfunction
  , pushhsfunction
  , pushrawhsfunction
  , registerhsfunction
  , registerrawhsfunction
  , mkWrapper
  ) where

import Control.Monad (when)
import Data.ByteString.Char8 (unpack)
import Foreign.C (CInt (..))
import Foreign.Lua.Api
import Foreign.Lua.Types
import Foreign.Lua.Util (getglobal')
import Foreign.Ptr (castPtr, freeHaskellFunPtr)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)

import qualified Foreign.Storable as F

-- | Type of raw haskell functions that can be made into 'CFunction's.
type PreCFunction = LuaState -> IO CInt

-- | Operations and functions that can be pushed to the lua stack. This is a
-- helper function not intended to be used directly. Use the @'luaimport'@
-- wrapper instead.
class LuaImport a where
  -- | Helper function, called by @'luaimport'@
  luaimport' :: StackIndex -> a -> Lua ()

instance ToLuaStack a => LuaImport (Lua a) where
  luaimport' _narg x = () <$ (x >>= push)

instance (FromLuaStack a, LuaImport b) => LuaImport (a -> b) where
  luaimport' narg f = getArg >>= luaimport' (narg + 1) . f
    where
      getArg = peek narg `catchLuaError` \err ->
        throwLuaError ("could not read argument "
                     ++ show (fromStackIndex narg) ++ ": " ++ show err)

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of @'FromLuaStack'@
--   * return type is @Lua a@, where @a@ is an instance of
--     @'ToLuaStack'@
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
luaimport :: LuaImport a => a -> Lua CInt
luaimport a = (1 <$ luaimport' 1 a) `catchLuaError` \err -> do
  push ("Error while calling haskell function: " ++ show err)
  fromIntegral <$> lerror

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newcfunction :: LuaImport a => a -> Lua CFunction
newcfunction = liftIO . mkWrapper . flip runLuaWith . luaimport

-- | Turn a @'PreCFunction'@ into an actual @'CFunction'@.
foreign import ccall "wrapper"
  mkWrapper :: PreCFunction -> IO CFunction

-- | Free function pointer created with @newcfunction@.
freecfunction :: CFunction -> Lua ()
freecfunction = liftIO . freeHaskellFunPtr

-- | Helper class used to make lua functions useable from haskell
class LuaCallFunc a where
  callfunc' :: String -> Lua () -> NumArgs -> a

instance (FromLuaStack a) => LuaCallFunc (Lua a) where
  callfunc' fnName x nargs = do
    getglobal' fnName
    x
    z <- pcall nargs 1 Nothing
    if z /= LuaOK
      then tostring (-1) >>= throwLuaError . unpack
      else peek (-1) <* pop 1

instance (ToLuaStack a, LuaCallFunc b) => LuaCallFunc (a -> b) where
  callfunc' fnName pushArgs nargs x =
    callfunc' fnName (pushArgs *> push x) (nargs + 1)

-- | Call a Lua function. Use as:
--
-- > v <- callfunc "proc" "abc" (1::Int) (5.0::Double)
callfunc :: (LuaCallFunc a) => String -> a
callfunc f = callfunc' f (return ()) 0


foreign export ccall hsmethod__gc :: LuaState -> IO CInt
foreign import ccall "&hsmethod__gc" hsmethod__gc_addr :: CFunction

foreign export ccall hsmethod__call :: LuaState -> IO CInt
foreign import ccall "&hsmethod__call" hsmethod__call_addr :: CFunction

hsmethod__gc :: LuaState -> IO CInt
hsmethod__gc l = do
  ptr <- runLuaWith l $ peek (-1)
  stableptr <- F.peek (castPtr ptr)
  freeStablePtr stableptr
  return 0

hsmethod__call :: LuaState -> IO CInt
hsmethod__call l = do
  ptr <- runLuaWith l $ peek 1 <* remove 1
  stableptr <- F.peek (castPtr ptr)
  f <- deRefStablePtr stableptr
  f l


-- | Pushes Haskell function converted to a Lua function.
-- All values created will be garbage collected. Use as:
--
-- > Lua.pushhsfunction myfun
-- > Lua.setglobal "myfun"
--
-- You are not allowed to use @lua_error@ anywhere, but
-- use an error code of (-1) to the same effect. Push
-- error message as the sole return value.
pushhsfunction :: LuaImport a => a -> Lua ()
pushhsfunction = pushrawhsfunction . flip runLuaWith . luaimport

-- | Pushes _raw_ Haskell function converted to a Lua function.
-- Raw Haskell functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left in the stack.
pushrawhsfunction :: PreCFunction -> Lua ()
pushrawhsfunction f = do
  stableptr <- liftIO $ newStablePtr f
  p <- newuserdata (F.sizeOf stableptr)
  liftIO $ F.poke (castPtr p) stableptr
  v <- newmetatable "HaskellImportedFunction"
  when v $ do
    -- create new metatable, fill it with two entries __gc and __call
    push hsmethod__gc_addr
    setfield (-2) "__gc"
    push hsmethod__call_addr
    setfield (-2) "__call"
  setmetatable (-2)
  return ()

-- | Imports a Haskell function and registers it at global name.
registerhsfunction :: LuaImport a => String -> a -> Lua ()
registerhsfunction n f = pushhsfunction f *> setglobal n

-- | Imports a raw Haskell function and registers it at global name.
registerrawhsfunction :: String -> PreCFunction -> Lua ()
registerrawhsfunction n f = pushrawhsfunction f *> setglobal n


