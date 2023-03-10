{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Module.Version
Copyright   : © 2019-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Lua module to work with file paths.
-}
module HsLua.Module.Version (
  -- * Module
    documentedModule
  -- * Version objects
  , typeVersion
  , peekVersion
  , pushVersion
  , peekVersionFuzzy
  )
where

import Prelude hiding (error)
import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import Data.Version
  ( Version, makeVersion, parseVersion, showVersion, versionBranch )
import Data.List.NonEmpty as NonEmpty (last, nonEmpty)
import Data.Text (Text)
import HsLua.Core
  ( LuaError, Type (..) , call, dostring, error, ltype )
import HsLua.Marshalling
  ( Peeker, Pusher, failPeek, liftLua, peekIntegral, peekList, peekString
  , pushIntegral, pushIterator, pushString, retrieving )
import HsLua.Packaging
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified HsLua.Core.Utf8 as UTF8

-- | The @path@ module specification.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "Version"
  , moduleDescription = "Version specifier handling"
  , moduleFields = []
  , moduleFunctions = [must_be_at_least]
  , moduleOperations =
    [ operation Call $ lambda
      ### liftPure2 (\_ v -> v)
      <#> parameter (const $ pure ()) "table" "module table" "ignored"
      <#> versionParam "version" "version-like object"
      =#> udresult typeVersion "new Version object"
    ]
  , moduleTypeInitializers = []
  }

-- | Type definition of Lua Version values.
typeVersion :: LuaError e => DocumentedTypeWithList e Version Int
typeVersion = deftype' "Version"
  [ operation Eq $ lambda
      ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
      <#> parameter (optional . peekVersionFuzzy) "Version" "a" ""
      <#> parameter (optional . peekVersionFuzzy) "Version" "b" ""
      =#> boolResult "true iff v1 == v2"
  , operation Le $ versionComparison (<=) "true iff v1 <= v2"
  , operation Lt $ versionComparison (<)  "true iff v1 < v2"
  , operation Len $ lambda
    ### liftPure (length . versionBranch)
    <#> versionParam "version" ""
    =#> integralResult "number of version components"
  , operation Pairs $ lambda
    ### pushIterator (\(i, n) -> 2 <$ pushIntegral i <* pushIntegral n)
          . zip [(1 :: Int) ..] . versionBranch
    <#> versionParam "version" ""
    =?> "iterator values"
  , operation Tostring $ lambda
    ### liftPure showVersion
    <#> versionParam "version" ""
    =#> stringResult "stringified version"
  ]
  [ method must_be_at_least ]
  (Just ( (pushIntegral, versionBranch)
        , (peekIntegral, const makeVersion)))
  where
    versionComparison f descr = lambda
      ### liftPure2 f
      <#> versionParam "v1" ""
      <#> versionParam "v2" ""
      =#> boolResult descr

-- | Push a @'Version'@ element to the Lua stack.
pushVersion :: LuaError e => Pusher e Version
pushVersion = pushUD typeVersion

-- | Retrieve a @'Version'@ object from the top of the stack.
peekVersion :: LuaError e => Peeker e Version
peekVersion = peekUD typeVersion

-- | Retrieve a Version-like object from the top of the stack.
--
-- This function uses these heuristics, depending on the Lua object
-- type.
--
--   * string: object is parsed as a version specifier.
--
--   * table: value is expected to be a list of integers, with each
--     index specifying a version branch.
--
--   * userdata: assumes the value to be a Version userdata object.
--
--   * number: parses the number as an integer value.
--
-- Otherwise, or if the object fails to meet an expectation, peeking
-- fails.
peekVersionFuzzy :: LuaError e => Peeker e Version
peekVersionFuzzy idx = retrieving "Version" $ liftLua (ltype idx) >>= \case
  TypeUserdata -> peekVersion idx
  TypeString   -> do
    versionStr <- peekString idx
    let parses = readP_to_S parseVersion versionStr
    case NonEmpty.last <$> NonEmpty.nonEmpty parses of
      Just (v, "") -> return v
      _  -> failPeek $
            "could not parse as Version: " <> UTF8.fromString versionStr
  TypeNumber   -> makeVersion . (:[]) <$> peekIntegral idx
  TypeTable    -> makeVersion <$> peekList peekIntegral idx
  _ ->
    failPeek "could not peek Version"

-- | Parameter that takes a Version-like object.
versionParam :: LuaError e => Text -> Text -> Parameter e Version
versionParam = parameter peekVersionFuzzy "Version"

-- | Throw an error if this version is older than the given version. This
-- function currently the string library to be loaded.
must_be_at_least :: LuaError e => DocumentedFunction e
must_be_at_least =
  defun "must_be_at_least"
    ### (\actual expected mMsg -> do
            -- Default error message when a version is too old. This
            -- message is formatted in Lua with the expected and actual
            -- versions as arguments.
            let versionTooOldMessage = "expected version %s or newer, got %s"
            let msg = fromMaybe versionTooOldMessage mMsg
            if expected <= actual
              then return 0
              else do
              _ <- dostring "return string.format"
              pushString msg
              pushString (showVersion expected)
              pushString (showVersion actual)
              call 3 1
              error)
    <#> versionParam "self" "version to check"
    <#> versionParam "reference" "minimum version"
    <#> opt (stringParam "msg" "alternative message")
    =?> mconcat [ "Returns no result, and throws an error if this "
                , "version is older than `reference`."
                ]
