{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua.Module.System
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Provide a Lua module containing a selection of @'System'@ functions.
-}
module Foreign.Lua.Module.System
  ( pushModule
  , preloadModule
  )
where

import Control.Applicative ((<$>))
import Control.Exception (IOException, catch, evaluate, try)
import Data.Maybe (fromMaybe)
import Foreign.Lua (Lua, NumResults(..), Optional (..), Peekable, Pushable,
                    StackIndex, ToHaskellFunction)
import System.IO.Error (IOError, isDoesNotExistError)

import qualified Foreign.Lua as Lua
import qualified System.Directory as Directory
import qualified System.IO.Temp as Temp

-- | Pushes the @text@ module to the lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "chdir" chdir
  addFunction "currentdir" currentdir
  addFunction "ls" ls
  addFunction "pwd" currentdir
  addFunction "tmpdirname" tmpdirname
  addFunction "with_tmpdir" with_tmpdir
  return 1

-- | Add the text module under the given name to the table of preloaded
-- packages.
preloadModule :: String -> Lua ()
preloadModule = flip addPackagePreloader pushModule

-- | Registers a preloading function. Takes an module name and the Lua
-- operation which produces the package.
addPackagePreloader :: String -> Lua NumResults -> Lua ()
addPackagePreloader name modulePusher = do
  Lua.getfield Lua.registryindex Lua.preloadTableRegistryField
  Lua.pushHaskellFunction modulePusher
  Lua.setfield (-2) name
  Lua.pop 1

-- | Attach a function to the table at the top of the stack, using the
-- given name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

-- | Lua callback function
newtype Callback = Callback { callbackStackIndex :: StackIndex }

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

with_tmpdir :: String            -- ^ parent dir or template
            -> AnyValue          -- ^ template or callback
            -> Optional Callback -- ^ callback or nil
            -> Lua NumResults
with_tmpdir parentDir tmpl callback = do
  case fromOptional callback of
    Nothing -> do
      -- At most two args. The first arg (parent dir) has probably been
      -- omitted, so we shift arguments and use the system's canonical
      -- temporary directory.
      let tmpl' = parentDir
      callback' <- Lua.peek (fromAnyValue tmpl)
      Temp.withSystemTempDirectory tmpl' (callWithFilename callback')
    Just callback' -> do
      -- all args given. Second value must be converted to a string.
      tmpl' <- Lua.peek (fromAnyValue tmpl)
      Temp.withTempDirectory parentDir tmpl' (callWithFilename callback')

-- | Call Lua callback function with the given filename as its argument.
callWithFilename :: Callback -> FilePath -> Lua NumResults
callWithFilename callback filename = do
  oldTop <- Lua.gettop
  Lua.push callback
  Lua.push filename
  Lua.call (Lua.NumArgs 1) Lua.multret
  newTop <- Lua.gettop
  return . NumResults . fromIntegral . Lua.fromStackIndex $
    newTop - oldTop

tmpdirname :: Lua FilePath
tmpdirname = do
  eitherTmpdir <- Lua.liftIO $ try Directory.getTemporaryDirectory
  case eitherTmpdir :: Either IOException FilePath of
    Right tmpdir -> return tmpdir
    Left _ -> Lua.throwException ("The operating system has no notion " ++
                                  "of temporary directory.")

-- | List the contents of a directory.
ls :: Optional FilePath -> Lua [FilePath]
ls fp = do
  let fp' = fromMaybe "." (fromOptional fp)
  ioToLua (Directory.listDirectory fp')

-- | Change current working directory.
chdir :: FilePath -> Lua ()
chdir fp = ioToLua $ Directory.setCurrentDirectory fp

-- | Return the current working directory.
currentdir :: Lua FilePath
currentdir = ioToLua Directory.getCurrentDirectory

-- | Convert a System IO operation to a Lua operation.
ioToLua :: IO a -> Lua a
ioToLua action = do
  result <- Lua.liftIO (try action)
  case result of
    Right result' -> return result'
    Left err      -> Lua.throwException (show (err :: IOException))
