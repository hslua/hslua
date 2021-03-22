{-|
Module      : HsLua.Module.SystemUtils
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Utility functions and types for HsLua's system module.
-}
module HsLua.Module.SystemUtils
  ( AnyValue (..)
  , Callback (..)
  , invoke
  , invokeWithFilePath
  , ioToLua
  )
where

import Control.Exception (IOException, try)
import HsLua.Core hiding (try)
import HsLua.Class.Peekable (Peekable (peek))
import HsLua.Class.Pushable (Pushable (push))

-- | Lua callback function. This type is similar to @'AnyValue'@, and
-- the same caveats apply.
newtype Callback = Callback StackIndex

instance Peekable Callback where
  peek idx = do
    isFn <- isfunction idx
    if isFn
      then return (Callback idx)
      else failLua "Function expected"

instance Pushable Callback where
  push (Callback idx) = pushvalue idx


-- | Any value of unknown type.
--
-- This simply wraps the function's index on the Lua stack. Changes to
-- the stack may only be made with great care, as they can break the
-- reference.
newtype AnyValue = AnyValue { fromAnyValue :: StackIndex }

instance Peekable AnyValue where
  peek = return . AnyValue

instance Pushable AnyValue where
  push (AnyValue idx) = pushvalue idx

-- | Call Lua callback function and return all of its results.
invoke :: Callback -> Lua NumResults
invoke callback = do
  oldTop <- gettop
  push callback
  call 0 multret
  newTop <- gettop
  return . NumResults . fromStackIndex $
    newTop - oldTop

-- | Call Lua callback function with the given filename as its argument.
invokeWithFilePath :: Callback -> FilePath -> Lua NumResults
invokeWithFilePath callback filename = do
  oldTop <- gettop
  push callback
  push filename
  call (NumArgs 1) multret
  newTop <- gettop
  return . NumResults . fromStackIndex $
    newTop - oldTop

-- | Convert a System IO operation to a Lua operation.
ioToLua :: IO a -> Lua a
ioToLua action = do
  result <- liftIO (try action)
  case result of
    Right result' -> return result'
    Left err      -> failLua (show (err :: IOException))
