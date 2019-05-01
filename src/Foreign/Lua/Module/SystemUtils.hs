{-|
Module      : Foreign.Lua.Module.SystemUtils
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Utility functions and types for HsLua's system module.
-}
module Foreign.Lua.Module.SystemUtils
  ( AnyValue (..)
  , Callback (..)
  , addPackagePreloader
  , addField
  , addFunction
  , invoke
  , invokeWithFilePath
  , ioToLua
  )
where

import Control.Exception (IOException, try)
import Foreign.Lua (Lua, NumResults(..), Peekable, Pushable,
                    StackIndex, ToHaskellFunction)

import qualified Foreign.Lua as Lua

-- | Registers a preloading function. Takes an module name and the Lua
-- operation which produces the package.
addPackagePreloader :: String -> Lua NumResults -> Lua ()
addPackagePreloader name modulePusher = do
  Lua.getfield Lua.registryindex Lua.preloadTableRegistryField
  Lua.pushHaskellFunction modulePusher
  Lua.setfield (-2) name
  Lua.pop 1

-- | Add a string-indexed field to the table at the top of the stack.
addField :: Pushable a => String -> a -> Lua ()
addField name value = do
  Lua.push name
  Lua.push value
  Lua.rawset (Lua.nthFromTop 3)

-- | Attach a function to the table at the top of the stack, using the
-- given name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

-- | Lua callback function
newtype Callback = Callback StackIndex

instance Peekable Callback where
  peek idx = do
    isFn <- Lua.isfunction idx
    if isFn
      then return (Callback idx)
      else Lua.throwException "Function expected"

instance Pushable Callback where
  push (Callback idx) = Lua.pushvalue idx


-- | Any value of unknown type
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
