{-# LANGUAGE CPP #-}
{-|
Module      : HsLua.Module.System
Copyright   : © 2019-2025 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : Requires GHC 8 or later.

Provide a Lua module containing a selection of @'System'@ functions.
-}
module HsLua.Module.System (

  -- * Module
    documentedModule

  -- ** Fields
  , arch
  , compiler_name
  , compiler_version
  , os

  -- ** Functions
  , cmd
  , cp
  , cputime
  , env
  , getenv
  , getwd
  , ls
  , mkdir
  , rm
  , rmdir
  , setenv
  , setwd
  , tmpdirname
  , with_env
  , with_tmpdir
  , with_wd
  , xdg
  )
where

import Control.Monad ((>=>), forM_)
import Control.Monad.Catch (bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Version (versionBranch)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging
import HsLua.Module.SystemUtils

import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as Utf8
import qualified System.CPUTime as CPUTime
import qualified System.Directory as Directory
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.Info as Info
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

-- | The "system" module.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "system"
  , moduleFields =
      [ arch
      , compiler_name
      , compiler_version
      , cputime_precision
      , os
      ]
  , moduleFunctions =
      [ cmd
      , cp
      , cputime
      , env
      , getenv
      , getwd
      , ls
      , mkdir
      , rm
      , rmdir
      , setenv
      , setwd
      , tmpdirname
      , with_env
      , with_tmpdir
      , with_wd
      , xdg
      ]
  , moduleOperations = []
  , moduleTypeInitializers = []
  , moduleDescription =
      "Access to the system's information and file functionality."
  }

--
-- Fields
--

-- | Module field containing the machine architecture on which the
-- program is running. Wraps @'Info.arch'@
arch :: Field e
arch = Field
  { fieldName = "arch"
  , fieldType = "string"
  , fieldDescription =
      "The machine architecture on which the program is running."
  , fieldPushValue = pushString Info.arch
  }

-- | Module field containing the Haskell implementation with which the
-- host program was compiled. Wraps @'Info.compilerName'@.
compiler_name :: Field e
compiler_name = Field
  { fieldName = "compiler_name"
  , fieldType = "string"
  , fieldDescription = "The Haskell implementation with which the host "
                       `T.append` "program was compiled."
  , fieldPushValue = pushString Info.compilerName
  }

-- | Module field containing the version of `compiler_name` with which
-- the host program was compiled.
compiler_version :: LuaError e => Field e
compiler_version = Field
  { fieldName = "compiler_version"
  , fieldType = "string"
  , fieldDescription = T.unwords
      [ "The Haskell implementation with which the host "
      , "program was compiled." ]
  , fieldPushValue = pushList pushIntegral $
                     versionBranch Info.compilerVersion
  }

-- | Field containing the smallest measurable difference in CPU time.
cputime_precision :: Field e
cputime_precision = Field
  { fieldName = "cputime_precision"
  , fieldType = "integer"
  , fieldDescription = T.unlines
      [ "The smallest measurable difference in CPU time that the"
      , "implementation can record, and is given as an integral number of"
      , "picoseconds."
      ]
  , fieldPushValue = pushIntegral CPUTime.cpuTimePrecision
  }

-- | Field containing the operating system on which the program is
-- running.
os :: Field e
os = Field
  { fieldName = "os"
  , fieldType = "string"
  , fieldDescription = T.unlines
    [ "The operating system on which the program is running."
    , "The most common values are `darwin` (macOS), `freebsd`, `linux`,"
    , "`linux-android`, `mingw32` (Windows), `netbsd`, `openbsd`."
    ]
  , fieldPushValue = pushString Info.os
  }


--
-- Functions
--

-- | Run a system command
cmd :: LuaError e => DocumentedFunction e
cmd = defun "cmd"
  ### (\command args minput opts -> do
          let input = fromMaybe "" minput
          let cp_opts = (Process.proc command args)
                        { Process.env = processOptsEnv =<< opts
                        , Process.cwd = processOptsCwd =<< opts
                        }
          liftIO $ Process.readCreateProcessWithExitCode cp_opts input)
  <#> filepathParam "command" "command to execute"
  <#> parameter (peekList peekString) "{string,...}" "args" "command arguments"
  <#> opt (parameter peekString "string" "input" "input on stdin")
  <#> opt (parameter peekProcessOptions "table" "opts" "process options")
  =#> (functionResult (pushExitCode . (\(a,_,_) -> a)) "integer|boolean"
                      "exit code – `false` on success, an integer otherwise" <>
       functionResult (pushString . \(_,b,_) -> b) "string" "stdout" <>
       functionResult (pushString . \(_,_,c) -> c) "string" "stderr")
  #? T.unlines
     [ "Executes a system command with the given arguments and `input`"
     , "on *stdin*."
     ]

-- | Copy a file
cp :: LuaError e => DocumentedFunction e
cp = defun "cp"
  ### (\src tgt -> ioToLua $ Directory.copyFile src tgt)
  <#> filepathParam "source" "source file"
  <#> filepathParam "target" "target destination"
  =#> []
  #? T.unlines
     [ "Copy a file with its permissions."
     , "If the destination file already exists, it is overwritten."
     ]

-- | Access the CPU time, e.g. for benchmarking.
cputime :: LuaError e => DocumentedFunction e
cputime = defun "cputime"
  ### ioToLua CPUTime.getCPUTime
  =#> functionResult pushIntegral "integer" "CPU time in picoseconds"
  #? T.unlines
     [ "Returns the number of picoseconds CPU time used by the current"
     , "program. The precision of this result may vary in different"
     , "versions and on different platforms."
     ]

-- | Retrieve the entire environment
env :: LuaError e => DocumentedFunction e
env = defun "env"
  ### ioToLua Env.getEnvironment
  =#> functionResult (pushKeyValuePairs pushString pushString) "table"
        "A table mapping environment variable names to their value."
  #? "Retrieves the entire environment as a string-indexed table."

-- | Return the current working directory as an absolute path.
getwd :: LuaError e => DocumentedFunction e
getwd = defun "getwd"
  ### ioToLua Directory.getCurrentDirectory
  =#> filepathResult "The current working directory."
  #? "Obtain the current working directory as an absolute path."

-- | Returns the value of an environment variable
getenv :: LuaError e => DocumentedFunction e
getenv = defun "getenv"
  ### ioToLua . Env.lookupEnv
  <#> parameter peekString "string" "var" "name of the environment"
  =#> functionResult (maybe pushnil pushString) "string or nil"
        "value of the variable, or nil if the variable is not defined."
  #? T.unwords
    [ "Return the value of the environment variable `var`, or `nil` "
    , "if there is no such value." ]

-- | List the contents of a directory.
ls :: LuaError e => DocumentedFunction e
ls = defun "ls"
  ### ioToLua . Directory.listDirectory . fromMaybe "."
  <#> opt (stringParam "directory"
           ("Path of the directory whose contents should be listed. "
            `T.append` "Defaults to `.`."))
  =#> functionResult (pushList pushString) "table"
        ("A table of all entries in `directory`, except for the "
          `T.append` "special entries (`.`  and `..`).")
  #? "List the contents of a directory."


-- | Create a new directory which is initially empty, or as near to
-- empty as the operating system allows.
--
-- If the optional second parameter is `false`, then create the new
-- directory only if it doesn't exist yet. If the parameter is `true`,
-- then parent directories are created as necessary.
mkdir :: LuaError e => DocumentedFunction e
mkdir = defun "mkdir"
  ### (\fp createParent ->
         if createParent == Just True
         then ioToLua (Directory.createDirectoryIfMissing True fp)
         else ioToLua (Directory.createDirectory fp))
  <#> filepathParam "dirname" "name of the new directory"
  <#> opt (boolParam "create_parent" "create parent directory if necessary")
  =#> []
  #? T.concat
       [ "Create a new directory which is initially empty, or as near "
       , "to empty as the operating system allows. The function throws "
       , "an error if the directory cannot be created, e.g., if the "
       , "parent directory does not exist or if a directory of the "
       , "same name is already present.\n"
       , "\n"
       , "If the optional second parameter is provided and truthy, "
       , "then all directories, including parent directories, are "
       , "created as necessary.\n"
       ]

-- | Remove a file.
rm :: LuaError e => DocumentedFunction e
rm = defun "rm"
  ### ioToLua . Directory.removeFile
  <#> filepathParam "filename" "file to remove"
  =#> []
  #? "Removes the directory entry for an existing file."

-- | Remove an existing directory.
rmdir :: LuaError e => DocumentedFunction e
rmdir = defun "rmdir"
  ### (\fp recursive ->
         if recursive == Just True
         then ioToLua (Directory.removeDirectoryRecursive fp)
         else ioToLua (Directory.removeDirectory fp))
  <#> filepathParam "dirname" "name of the directory to delete"
  <#> opt (boolParam "recursive" "delete content recursively")
  =#> []
  #?("Remove an existing, empty directory. If `recursive` is given, "
     `T.append` "then delete the directory and its contents recursively.")

-- | Set the specified environment variable to a new value.
setenv :: LuaError e => DocumentedFunction e
setenv = defun "setenv"
  ### (\name value -> ioToLua (Env.setEnv name value))
  <#> parameter peekString "string" "name"
        "name of the environment variable"
  <#> parameter peekString "string" "value" "new value"
  =#> []
  #? "Set the specified environment variable to a new value."

-- | Change current working directory.
setwd :: LuaError e => DocumentedFunction e
setwd = defun "setwd"
  ### ioToLua . Directory.setCurrentDirectory
  <#> filepathParam "directory" "Path of the new working directory"
  =#> []
  #? "Change the working directory to the given path."

-- | Get the current directory for temporary files.
tmpdirname :: LuaError e => DocumentedFunction e
tmpdirname = defun "tmpdirname"
  ### ioToLua Directory.getTemporaryDirectory
  =#> functionResult pushString "string"
        "The current directory for temporary files."
  #? mconcat
     [ "Returns the current directory for temporary files.\n"
     , "\n"
     , "On Unix, `tmpdirname()` returns the value of the `TMPDIR` "
     , "environment variable or \"/tmp\" if the variable isn't defined. "
     , "On Windows, the function checks for the existence of environment "
     , "variables in the following order and uses the first path found:\n"
     , "\n"
     , "- TMP environment variable.\n"
     , "- TEMP environment variable.\n"
     , "- USERPROFILE environment variable.\n"
     , "- The Windows directory\n"
     , "\n"
     , "The operation may fail if the operating system has no notion of "
     , "temporary directory.\n"
     , "\n"
     , "The function doesn't verify whether the path exists.\n"
     ]

-- | Run an action in a different directory, then restore the old
-- working directory.
with_wd :: LuaError e => DocumentedFunction e
with_wd = defun "with_wd"
  ### (\fp callback ->
        bracket (ioToLua Directory.getCurrentDirectory)
                (ioToLua . Directory.setCurrentDirectory)
           (\_ -> do
              ioToLua (Directory.setCurrentDirectory fp)
              callback `invokeWithFilePath` fp))
  <#> filepathParam "directory"
        "Directory in which the given `callback` should be executed"
  <#> parameter peekCallback "function" "callback"
        "Action to execute in the given directory"
  =?> "The results of the call to `callback`."
  #? T.unwords
     [ "Run an action within a different directory. This function will"
     , "change the working directory to `directory`, execute `callback`,"
     , "then switch back to the original working directory, even if an"
     , "error occurs while running the callback action."
     ]

-- | Run an action, then restore the old environment variable values.
with_env :: LuaError e => DocumentedFunction e
with_env = defun "with_env"
  ### (\environment callback ->
        bracket (ioToLua Env.getEnvironment)
                setEnvironment
                (\_ -> setEnvironment environment *> invoke callback))
  <#> parameter (peekKeyValuePairs peekString peekString) "table"
        "environment"
        ("Environment variables and their values to be set before "
         `T.append` "running `callback`")
  <#> parameter peekCallback "function" "callback"
        "Action to execute in the custom environment"
  =?> "The results of the call to `callback`."
  #? T.unwords
     [ "Run an action within a custom environment. Only the environment"
     , "variables given by `environment` will be set, when `callback` is"
     , "called. The original environment is restored after this function"
     , "finishes, even if an error occurs while running the callback"
     , "action."
     ]
 where
  setEnvironment newEnv = ioToLua $ do
    -- Crude, but fast enough: delete all entries in new environment,
    -- then restore old environment one-by-one.
    curEnv <- Env.getEnvironment
    forM_ curEnv (Env.unsetEnv . fst)
    forM_ newEnv (uncurry Env.setEnv)

-- | Provides a temporary directory for the given action.
with_tmpdir :: LuaError e => DocumentedFunction e
with_tmpdir = defun "with_tmpdir"
  ### (\mParentDir tmpl callback -> case mParentDir of
          Nothing -> do
            Temp.withSystemTempDirectory tmpl $
              invokeWithFilePath callback
          Just parentDir -> do
            Temp.withTempDirectory parentDir tmpl $
              invokeWithFilePath callback)
  <#> parameter peekParentDir "string" "parent_dir"
        (mconcat
         [ "Parent directory to create the directory in. If this "
         , "parameter is omitted, the system's canonical temporary "
         , "directory is used."
         ])
  <#> stringParam "templ" "Directory name template."
  <#> parameter peekCallback "function" "callback"
        ("Function which takes the name of the temporary directory as "
         `T.append` "its first argument.")
  =?> "The results of the call to `callback`."
  #? T.unlines
     [ "Create and use a temporary directory inside the given directory."
     , "The directory is deleted after the callback returns."
     ]
  where
    peekParentDir idx = do
      args <- liftLua gettop
      if args < 3
        then liftLua $ do
          pushnil
          insert idx
          return Nothing
        else Just <$> peekString idx

-- | Obtain the paths to special directories.
xdg :: LuaError e => DocumentedFunction e
xdg = defun "xdg"
  ### (\xdgDirTypeOrList mfp->
         case xdgDirTypeOrList of
           Left xdgDirType -> Left <$>
             let fp = fromMaybe "" mfp
             in ioToLua $ Directory.getXdgDirectory xdgDirType fp
           Right xdgDirList ->
             ioToLua $ Right <$> Directory.getXdgDirectoryList xdgDirList)

  <#> parameter peekXdgDirectory "string" "xdg_directory_type"
        (T.unlines
         [ "The type of the XDG directory or search path."
         , "Must be one of `config`, `data`, `cache`, `state`,"
         , "`datadirs`, or `configdirs`."
         , ""
         , "Matching is case-insensitive, and underscores and `XDG`"
         , "prefixes are ignored, so a value like"
         , "`XDG_DATA_DIRS` is also acceptable."
         , ""
         , "The `state` directory might not be available, depending"
         , "on the version of the underlying Haskell library."
         ])
  <#> opt (filepathParam "filepath"
           ("relative path that is appended to the path; ignored " <>
            "if the result is a list of search paths."))
  =#> functionResult (either pushString (pushList pushString))
        "string|{string,...}"
        "Either a single file path, or a list of search paths."
  #? T.unlines
     [ "Access special directories and directory search paths."
     , ""
     , "Special directories for storing user-specific application"
     , "data, configuration, and cache files, as specified by the"
     , "[XDG Base Directory Specification](" <>
       "https://specifications.freedesktop.org/basedir-spec/latest/)."
     ]

--
-- Parameters
--

-- | Filepath function parameter.
filepathParam :: Text  -- ^ name
              -> Text  -- ^ description
              -> Parameter e FilePath
filepathParam = stringParam

-- | Result of a function returning a file path.
filepathResult :: Text -- ^ Description
               -> [FunctionResult e FilePath]
filepathResult = functionResult pushString "string"

--
-- Process parameters
--

-- | Process options
data ProcessOpts = ProcessOpts
  { processOptsEnv  :: Maybe [(String, String)]
  , processOptsCwd  :: Maybe FilePath
  }

-- | Peek process creation options
peekProcessOptions :: LuaError e => Peeker e ProcessOpts
peekProcessOptions = typeChecked "table" istable $ \idx -> do
  let peekEnv = peekKeyValuePairs peekString peekString
  env' <- peekFieldRaw (peekNilOr peekEnv)    "env" idx
  cwd' <- peekFieldRaw (peekNilOr peekString) "cwd" idx
  return $ ProcessOpts
    { processOptsEnv = env'
    , processOptsCwd = cwd'
    }

-- | Pushes an exit code; failure codes are pushed as integers, and
-- success is pushed as `false`. This means that the value can be
-- interpreted as a boolean `failed` value.
pushExitCode :: Pusher e Exit.ExitCode
pushExitCode = \case
  Exit.ExitSuccess   -> pushBool False
  Exit.ExitFailure n -> pushIntegral n

-- | Get an XDG directory type identifier.
peekXdgDirectory :: Peeker e
                    (Either Directory.XdgDirectory Directory.XdgDirectoryList)
peekXdgDirectory =
  (fmap cleanupXdgSpec . peekText) >=> \case
    "cache"       -> pure (Left Directory.XdgCache)
    "config"      -> pure (Left Directory.XdgConfig)
    "data"        -> pure (Left Directory.XdgData)
#if MIN_VERSION_directory(1,3,7)
    "state"       -> pure (Left Directory.XdgState)
#endif
    "datadirs"    -> pure (Right Directory.XdgDataDirs)
    "configdirs"  -> pure (Right Directory.XdgConfigDirs)
    s             -> failPeek $
      "Expected 'cache', 'config', 'data', or 'state', got: " <>
      Utf8.fromText s
  where
    -- Cleanup the XDG directory specifier as to make matching easier
    -- while keeping things permissive.
    -- Remove underscores, any 'xdg' prefix, and
    -- make sure everything is lowercase
    cleanupXdgSpec =
      (\s -> fromMaybe s $ T.stripPrefix "xdg" s)
      . T.filter (/= '_')
      . T.toLower
