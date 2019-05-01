{-|
Module      : Foreign.Lua.Module.System
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Provide a Lua module containing a selection of @'System'@ functions.
-}
module Foreign.Lua.Module.System
  ( pushModule
  , preloadModule
  )
where

import Control.Applicative ((<$>))
import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Control.Monad.Catch (bracket)
import Data.Maybe (fromMaybe)
import Data.Version (versionBranch)
import Foreign.Lua (Lua, NumResults(..), Optional (..), Peekable, Pushable,
                    StackIndex, ToHaskellFunction)

import qualified Data.Map as Map
import qualified Foreign.Lua as Lua
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Info as Info
import qualified System.IO.Temp as Temp

-- | Pushes the @system@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addField "arch" Info.arch
  addField "compiler_name" Info.compilerName
  addField "compiler_version" (versionBranch Info.compilerVersion)
  addField "os" Info.os
  addFunction "chdir" chdir
  addFunction "currentdir" currentdir
  addFunction "env" env
  addFunction "getenv" getenv
  addFunction "ls" ls
  addFunction "mkdir" mkdir
  addFunction "rmdir" rmdir
  addFunction "setenv" setenv
  addFunction "tmpdirname" tmpdirname
  addFunction "with_env" with_env
  addFunction "with_tmpdir" with_tmpdir
  return 1

-- | Add the @system@ module under the given name to the table of
-- preloaded packages.
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

-- | Run an action, then restore the old environment variable values.
with_env :: Map.Map String String -> Callback -> Lua NumResults
with_env environment callback =
  bracket (Lua.liftIO Env.getEnvironment)
          setEnvironment
          (\_ -> setEnvironment (Map.toList environment) >> invoke callback)
 where
  setEnvironment newEnv = Lua.liftIO $ do
    -- Crude, but fast enough: delete all entries in new environment,
    -- then restore old environment one-by-one.
    curEnv <- Env.getEnvironment
    forM_ curEnv (Env.unsetEnv . fst)
    forM_ newEnv (uncurry Env.setEnv)

with_tmpdir :: String            -- ^ parent dir or template
            -> AnyValue          -- ^ template or callback
            -> Optional Callback -- ^ callback or nil
            -> Lua NumResults
with_tmpdir parentDir tmpl callback =
  case fromOptional callback of
    Nothing -> do
      -- At most two args. The first arg (parent dir) has probably been
      -- omitted, so we shift arguments and use the system's canonical
      -- temporary directory.
      let tmpl' = parentDir
      callback' <- Lua.peek (fromAnyValue tmpl)
      Temp.withSystemTempDirectory tmpl' (invokeWithFilePath callback')
    Just callback' -> do
      -- all args given. Second value must be converted to a string.
      tmpl' <- Lua.peek (fromAnyValue tmpl)
      Temp.withTempDirectory parentDir tmpl' (invokeWithFilePath callback')

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

-- | Retrieve the entire environment
env :: Lua NumResults
env = do
  kvs <- ioToLua Env.getEnvironment
  let addValue (k, v) = Lua.push k *> Lua.push v *> Lua.rawset (-3)
  Lua.newtable
  mapM_ addValue kvs
  return (NumResults 1)

-- | Returns the value of an environment variable
getenv :: String -> Lua (Optional String)
getenv name = ioToLua (Optional <$> Env.lookupEnv name)

-- | Create a new directory which is initially empty, or as near to
-- empty as the operating system allows.
--
-- If the optional second parameter is `false`, then create the new
-- directory only if it doesn't exist yet. If the parameter is `true`,
-- then parent directories are created as necessary.
mkdir :: FilePath -> Bool -> Lua ()
mkdir fp createParent =
  if createParent
  then ioToLua (Directory.createDirectoryIfMissing True fp)
  else ioToLua (Directory.createDirectory fp)

-- | Remove an existing directory.
rmdir :: FilePath -> Bool -> Lua ()
rmdir fp recursive =
  if recursive
  then ioToLua (Directory.removeDirectoryRecursive fp)
  else ioToLua (Directory.removeDirectory fp)

-- | Set the specified environment variable to a new value.
setenv :: String -> String -> Lua ()
setenv name value = ioToLua (Env.setEnv name value)

-- | Get the current directory for temporary files.
tmpdirname :: Lua FilePath
tmpdirname = ioToLua Directory.getTemporaryDirectory

-- | Convert a System IO operation to a Lua operation.
ioToLua :: IO a -> Lua a
ioToLua action = do
  result <- Lua.liftIO (try action)
  case result of
    Right result' -> return result'
    Left err      -> Lua.throwException (show (err :: IOException))
