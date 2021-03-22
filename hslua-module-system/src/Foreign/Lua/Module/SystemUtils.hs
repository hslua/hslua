{-|
Module      : Foreign.Lua.Module.SystemUtils
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Utility functions and types for HsLua's system module.
-}
module Foreign.Lua.Module.SystemUtils
  ( AnyValue (..)
  , Callback (..)
  , invoke
  , invokeWithFilePath
  , ioToLua
  )
where

import Control.Exception (IOException, try)
import Foreign.Lua (Lua, NumResults(..), Peekable, Pushable, StackIndex)
import qualified Foreign.Lua as Lua

-- | Lua callback function. This type is similar to @'AnyValue'@, and
-- the same caveats apply.
newtype Callback = Callback StackIndex

instance Peekable Callback where
  peek idx = do
    isFn <- Lua.isfunction idx
    if isFn
      then return (Callback idx)
      else Lua.throwException "Function expected"

instance Pushable Callback where
  push (Callback idx) = Lua.pushvalue idx


-- | Any value of unknown type.
--
-- This simply wraps the function's index on the Lua stack. Changes to
-- the stack may only be made with great care, as they can break the
-- reference.
newtype AnyValue = AnyValue { fromAnyValue :: StackIndex }

instance Peekable AnyValue where
  peek = return . AnyValue

instance Pushable AnyValue where
  push (AnyValue idx) = Lua.pushvalue idx

-- | Call Lua callback function and return all of its results.
invoke :: Callback -> Lua NumResults
invoke callback = do
  oldTop <- Lua.gettop
  Lua.push callback
  Lua.call 0 Lua.multret
  newTop <- Lua.gettop
  return . NumResults . fromIntegral . Lua.fromStackIndex $
    newTop - oldTop

-- | Call Lua callback function with the given filename as its argument.
invokeWithFilePath :: Callback -> FilePath -> Lua NumResults
invokeWithFilePath callback filename = do
  oldTop <- Lua.gettop
  Lua.push callback
  Lua.push filename
  Lua.call (Lua.NumArgs 1) Lua.multret
  newTop <- Lua.gettop
  return . NumResults . fromIntegral . Lua.fromStackIndex $
    newTop - oldTop

-- | Convert a System IO operation to a Lua operation.
ioToLua :: IO a -> Lua a
ioToLua action = do
  result <- Lua.liftIO (try action)
  case result of
    Right result' -> return result'
    Left err      -> Lua.throwException (show (err :: IOException))
