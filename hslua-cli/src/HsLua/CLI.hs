{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Lua
Copyright   : Copyright © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

Embeddable Lua interpreter interface.
-}
module HsLua.CLI
  ( -- * Run scripts as program
    runStandalone
  , Settings (..)
  ) where

import Control.Monad (unless, when, zipWithM_)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Foreign.Ptr (nullPtr)
import HsLua.Core (LuaE, LuaError)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName, lookupEnv)
import System.IO (hPutStrLn, stderr)
import qualified Lua.Auxiliary as Lua
import qualified Lua.Constants as Lua
import qualified HsLua.Core as Lua
import qualified HsLua.Marshalling as Lua
import qualified HsLua.Core.Types as Lua
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified HsLua.Core.Utf8 as UTF8

-- | Settings for the Lua command line interface.
data Settings e = Settings
  { settingsVersionInfo :: Text
  , settingsRunner      :: LuaE e () -> IO ()
  }

-- | Get the Lua interpreter options from the command line. Throws an
-- error with usage instructions if parsing fails.
getOptions :: IO Options
getOptions = do
  rawArgs <- getArgs
  progName <- getProgName
  let (actions, args, errs) = getOpt RequireOrder luaOptions rawArgs
  unless (null errs) . ioError . userError $
    let usageHead = "Usage: " ++ progName ++ " [options] [script [args]]"
    in concat errs ++ usageInfo usageHead luaOptions

  let (mscript, arg) = first listToMaybe $ splitAt 1 args
  let opts = foldl' (flip ($)) defaultLuaOpts actions
  return opts
    { optScript = mscript
    , optScriptArgs = arg
    , optProgName = progName
    , optAllArgs = rawArgs
    }

-- | Print version information to the terminal.
showVersion :: LuaError e => Text -> LuaE e ()
showVersion extraInfo = do
  _ <- Lua.getglobal "_VERSION"
  versionString <- Lua.forcePeek $ Lua.peekText Lua.top
  Lua.liftIO . T.putStrLn $ versionString `T.append` extraInfo

-- | Runs code given on the command line
runCode :: LuaError e => LuaCode -> LuaE e ()
runCode = \case
  ExecuteCode stat -> do
    status <- Lua.dostringTrace stat
    when (status /= Lua.OK)
      Lua.throwErrorAsException
  RequireModule g mod' -> do
    _ <- Lua.getglobal "require"
    Lua.pushName mod'
    status <- Lua.pcallTrace 1 1
    if status == Lua.OK
      then Lua.setglobal g
      else Lua.throwErrorAsException

-- | Uses the first command line argument as the name of a script file
-- and tries to run that script in Lua. Falls back to stdin if no file
-- is given. Any remaining args are passed to Lua via the global table
-- @arg@.
runStandalone :: LuaError e => Settings e -> IO ()
runStandalone settings = do
  opts <- getOptions
  settingsRunner settings $ do
    let putErr = Lua.liftIO . hPutStrLn stderr
    -- print version info
    when (optVersion opts) (showVersion $ settingsVersionInfo settings)
    when (optInteractive opts) $
      putErr "[WARNING] Flag `-i` is not supported yet."
    when (optNoEnv opts) $
      putErr "[WARNING] Flag `-E` is not fully supported yet."
    when (optWarnings opts) $
      putErr "[WARNING] Flag `-W` is not supported yet."

    -- push `arg` table
    case optScript opts of
      Just _script -> do
        let setField i x = Lua.pushString x *> Lua.rawseti (Lua.nth 2) i
        let nprogargs = length (optAllArgs opts) - length (optScriptArgs opts)
        let arg = optProgName opts : optAllArgs opts
        Lua.newtable
        zipWithM_ setField [-(fromIntegral nprogargs)..] arg
      Nothing -> do
        Lua.pushList Lua.pushString (optAllArgs opts)
        Lua.pushString (optProgName opts)
        Lua.rawseti (Lua.nth 2) 0
    Lua.setglobal "arg"

    unless (optNoEnv opts) $ do
      init' <- Lua.liftIO $ lookupEnv "LUA_INIT"
      (case init' of
         Just ('@' : filename) -> Lua.dofileTrace filename
         Just cmd              -> Lua.dostring (UTF8.fromString cmd)
         Nothing               -> return Lua.OK)
        >>= \case
        Lua.OK -> pure ()
        _      -> Lua.throwErrorAsException

    -- run code statements and module loading instructions
    mapM_ runCode (reverse $ optExecute opts)

    let nargs = fromIntegral . length $ optScriptArgs opts
    result <- case optScript opts of
      -- `dofileTrace` should really accept a (Maybe FilePath)
      Just script | script /= "-" -> do
        Lua.loadfile script >>= \case
          Lua.OK -> do
            mapM_ Lua.pushString (optScriptArgs opts)
            Lua.pcallTrace nargs Lua.multret
          s      -> pure s
      Nothing | optVersion opts || not (null (optExecute opts)) ->
        pure Lua.OK
      _ -> do
        -- load script from stdin
        l <- Lua.state
        Lua.liftIO (Lua.luaL_loadfile l nullPtr) >>= \case
          Lua.LUA_OK -> do
            mapM_ Lua.pushString (optScriptArgs opts)
            Lua.pcallTrace nargs Lua.multret
          s          -> pure $ Lua.toStatus s

    when (result /= Lua.OK)
      Lua.throwErrorAsException

-- | Code to execute on startup.
data LuaCode =
    ExecuteCode ByteString
  | RequireModule Lua.Name Lua.Name

-- | Lua runner command line options.
data Options = Options
  { optNoEnv       :: Bool          -- ^ Ignore environment variables
  , optInteractive :: Bool          -- ^ Interactive
  , optVersion     :: Bool          -- ^ Show version info
  , optWarnings    :: Bool          -- ^ Whether warnings are enabled
  , optExecute     :: [LuaCode]     -- ^ code to execute, in reverse order
  , optProgName    :: String        -- ^ program name
  , optAllArgs     :: [String]      -- ^ all arguments
  , optScript      :: Maybe String  -- ^ script name, if any
  , optScriptArgs  :: [String]      -- ^ arguments passed to the script
  }

defaultLuaOpts :: Options
defaultLuaOpts = Options
  { optNoEnv = False
  , optInteractive = False
  , optVersion = False
  , optWarnings = False
  , optExecute = mempty
  , optProgName = mempty
  , optAllArgs = mempty
  , optScript = Nothing
  , optScriptArgs = mempty
  }

-- | Lua command line options.
luaOptions :: [OptDescr (Options -> Options)]
luaOptions =
  [ Option "e" []
    (flip ReqArg "stat" $ \stat opt ->
        let code = ExecuteCode $ UTF8.fromString stat
        in opt{ optExecute = code:optExecute opt })
    "execute string 'stat'"

  , Option "i" []
    (NoArg $ \opt -> opt { optInteractive = True })
    "interactive mode -- currently not supported"

  , Option "l" []
    (flip ReqArg "mod" $ \mod' opt ->
      let toName = Lua.Name . UTF8.fromString
          code = case break (== '=') mod' of
            (glb, '=':m)  -> RequireModule (toName glb) (toName m)
            (glb, _    )  -> RequireModule (toName glb) (toName glb)
      in opt{ optExecute = code:optExecute opt })
    (unlines
     [ "require library 'mod' into global 'mod';"
     , "if 'mod' has the pattern 'g=module', then"
     , "require library 'module' into global 'g'"
     ])

  , Option "v" []
    (NoArg $ \opt -> opt { optVersion = True })
    "show version information"

  , Option "E" []
    (NoArg $ \opt -> opt { optNoEnv = True })
    "ignore environment variables -- partially supported"

  , Option "W" []
    (NoArg $ \opt -> opt { optWarnings = True })
    "turn warnings on -- currently not supported"
  ]
