{-|
Module      : HsLua.Module.SystemUtils
Copyright   : © 2019-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

Utility functions and types for HsLua's system module.
-}
module HsLua.Module.SystemUtils
  ( Callback (..)
  , peekCallback
  , invoke
  , invokeWithFilePath
  , ioToLua
  )
where

import Control.Exception (IOException, try)
import HsLua.Core hiding (try)
import HsLua.Marshalling

-- | Lua callback function. This type is similar to @'AnyValue'@, and
-- the same caveats apply.
newtype Callback = Callback StackIndex

peekCallback :: Peeker e Callback
peekCallback = reportValueOnFailure "function" $ \idx -> do
  idx' <- absindex idx
  isFn <- isfunction idx'
  return $ if isFn
           then Just $ Callback idx'
           else Nothing

pushCallback :: Pusher e Callback
pushCallback (Callback idx) = pushvalue idx

-- | Call Lua callback function and return all of its results.
invoke :: LuaError e
       => Callback -> LuaE e NumResults
invoke callback = do
  oldTop <- gettop
  pushCallback callback
  call 0 multret
  newTop <- gettop
  return . NumResults . fromStackIndex $
    newTop - oldTop

-- | Call Lua callback function with the given filename as its argument.
invokeWithFilePath :: LuaError e
                   => Callback -> FilePath -> LuaE e NumResults
invokeWithFilePath callback filename = do
  oldTop <- gettop
  pushCallback callback
  pushString filename
  call (NumArgs 1) multret
  newTop <- gettop
  return . NumResults . fromStackIndex $
    newTop - oldTop

-- | Convert a System IO operation to a Lua operation.
ioToLua :: LuaError e => IO a -> LuaE e a
ioToLua action = do
  result <- liftIO (try action)
  case result of
    Right result' -> return result'
    Left err      -> failLua (show (err :: IOException))
