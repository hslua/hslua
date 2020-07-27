{-|
Module      : Foreign.Lua.Module.System
Copyright   : © 2019 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Provide a Lua module containing a selection of @'System'@ functions.
-}
module Foreign.Lua.Module.System (

  -- * Module
    pushModule
  , preloadModule

  -- * Fields
  , arch
  , compiler_name
  , compiler_version
  , os

  -- * Functions
  , env
  , getwd
  , getenv
  , ls
  , mkdir
  , rmdir
  , setenv
  , setwd
  , tmpdirname
  , with_env
  , with_tmpdir
  , with_wd

  -- * Path manipulations
  , take_directory
  , take_filename
  , take_extensions
  , split_directories
  , has_extension
  , drop_extensions
  , join_path
  , is_relative
  , is_absolute
  , normalise
  )
where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Catch (bracket)
import Data.Maybe (fromMaybe)
import Data.Version (versionBranch)
import Foreign.Lua (Lua, NumResults (..), Optional (..))
import Foreign.Lua.Module.SystemUtils

import qualified Data.Map as Map
import qualified Foreign.Lua as Lua
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Info as Info
import qualified System.IO.Temp as Temp
import qualified System.FilePath as Fp

--
-- Module
--

-- | Pushes the @system@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  Lua.addfield "arch" arch
  Lua.addfield "compiler_name" compiler_name
  Lua.addfield "compiler_version" compiler_version
  Lua.addfield "os" os
  Lua.addfunction "env" env
  Lua.addfunction "getenv" getenv
  Lua.addfunction "getwd" getwd
  Lua.addfunction "ls" ls
  Lua.addfunction "mkdir" mkdir
  Lua.addfunction "rmdir" rmdir
  Lua.addfunction "setenv" setenv
  Lua.addfunction "setwd" setwd
  Lua.addfunction "tmpdirname" tmpdirname
  Lua.addfunction "with_env" with_env
  Lua.addfunction "with_tmpdir" with_tmpdir
  Lua.addfunction "with_wd" with_wd
  -- * Path manipulations
  Lua.addfunction "take_directory"    take_directory
  Lua.addfunction "take_filename"     take_filename
  Lua.addfunction "take_extensions"   take_extensions
  Lua.addfunction "split_directories" split_directories
  Lua.addfunction "has_extension"     has_extension
  Lua.addfunction "drop_extensions"   drop_extensions
  Lua.addfunction "join_path"         join_path
  Lua.addfunction "is_relative"       is_relative
  Lua.addfunction "is_absolute"       is_absolute
  Lua.addfunction "normalise"         normalise
  return 1

-- | Add the @system@ module under the given name to the table of
-- preloaded packages.
preloadModule :: String -> Lua ()
preloadModule = flip Lua.preloadhs pushModule

--
-- Fields
--

-- | The machine architecture on which the program is running.
arch :: String
arch = Info.arch

-- | The Haskell implementation with which the host program was
-- compiled.
compiler_name :: String
compiler_name = Info.compilerName

-- | The version of `compiler_name` with which the host program was
-- compiled.
compiler_version :: [Int]
compiler_version = versionBranch Info.compilerVersion

-- | The operating system on which the program is running.
os :: String
os = Info.os


--
-- Functions
--

-- | Retrieve the entire environment
env :: Lua NumResults
env = do
  kvs <- ioToLua Env.getEnvironment
  let addValue (k, v) = Lua.push k *> Lua.push v *> Lua.rawset (-3)
  Lua.newtable
  mapM_ addValue kvs
  return (NumResults 1)

-- | Return the current working directory as an absolute path.
getwd :: Lua FilePath
getwd = ioToLua Directory.getCurrentDirectory

-- | Returns the value of an environment variable
getenv :: String -> Lua (Optional String)
getenv name = ioToLua (Optional <$> Env.lookupEnv name)

-- | List the contents of a directory.
ls :: Optional FilePath -> Lua [FilePath]
ls fp = do
  let fp' = fromMaybe "." (fromOptional fp)
  ioToLua (Directory.listDirectory fp')

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

-- | Change current working directory.
setwd :: FilePath -> Lua ()
setwd fp = ioToLua $ Directory.setCurrentDirectory fp

-- | Get the current directory for temporary files.
tmpdirname :: Lua FilePath
tmpdirname = ioToLua Directory.getTemporaryDirectory

-- | Run an action in a different directory, then restore the old
-- working directory.
with_wd :: FilePath -> Callback -> Lua NumResults
with_wd fp callback =
  bracket (Lua.liftIO Directory.getCurrentDirectory)
          (Lua.liftIO . Directory.setCurrentDirectory)
          $ \_ -> do
              Lua.liftIO (Directory.setCurrentDirectory fp)
              callback `invokeWithFilePath` fp


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

--
-- Path manipulations
--

-- | See @System.FilePath.takeDirectory@
take_directory :: FilePath -> Lua FilePath
take_directory fp = return (Fp.takeDirectory fp)

-- | See @System.FilePath.takeFilename@
take_filename :: FilePath -> Lua FilePath
take_filename fp = return (Fp.takeFileName fp)

-- | See @System.FilePath.takeExtensions@
take_extensions :: FilePath -> Lua String
take_extensions fp = return (Fp.takeExtensions fp)

-- | See @System.FilePath.dropExtension@
drop_extensions :: FilePath -> Lua String
drop_extensions fp = return (Fp.dropExtensions fp)

-- | See @System.FilePath.hasExtension@
has_extension :: FilePath -> Lua Bool
has_extension fp = return (Fp.hasExtension fp)

-- | See @System.FilePath.splitDirectories@
split_directories :: FilePath -> Lua [FilePath]
split_directories fp = return (Fp.splitDirectories fp)

-- | See @System.FilePath.joinPath@
join_path :: [FilePath] -> Lua FilePath
join_path fps = return (Fp.joinPath fps)

-- | See @System.FilePath.isRelative@
is_relative :: FilePath -> Lua Bool
is_relative fp = return (Fp.isRelative fp)

-- | See @System.FilePath.isAbsolute@
is_absolute :: FilePath -> Lua Bool
is_absolute fp = return (Fp.isAbsolute fp)

-- | See @System.FilePath.normalise@
normalise :: FilePath -> Lua FilePath
normalise fp = return (Fp.normalise fp)
