{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Module.Zip
Copyright   : © 2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Lua module to work with file zips.
-}
module HsLua.Module.Zip (
  -- * Module
    documentedModule

  -- * Fields

  -- * Zip archives
  )
where

import Codec.Archive.Zip (Archive)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Version (Version, makeVersion)
import HsLua.Core (LuaError, failLua)
import HsLua.Marshalling (peekLazyByteString, pushLazyByteString)
import HsLua.Packaging

import qualified Codec.Archive.Zip as Zip
import qualified Data.Text as T

-- | The @zip@ module specification.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "zip"
  , moduleDescription = "Module for zip archive handling."
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations = []
  }

-- | First published version of this library.
initialVersion :: Version
initialVersion = makeVersion [0,1,0]

--
-- Fields
--

-- | Exported fields.
fields :: [Field e]
fields = []


--
-- Functions
--

functions :: LuaError e => [DocumentedFunction e]
functions =
  [ toarchive
  , create
  ]

-- | Wrapper for 'Zip.toArchive'; converts a string into an Archive.
toarchive :: LuaError e => DocumentedFunction e
toarchive = defun "toarchive"
  ### (\str -> case Zip.toArchiveOrFail str of
          Left err -> failLua err
          Right a  -> return a
          )
  <#> parameter peekLazyByteString "string" "binary archive string" ""
  =#> udresult typeArchive ""
  #? T.unlines
     [ "Reads an *Archive* structure from a raw zip archive;"
     , "throws an error if the given string cannot be decoded into an archive."
     ]
  `since` initialVersion

-- | Wrapper for
create :: LuaError e => DocumentedFunction e
create = defun "create"
  ### return Zip.emptyArchive
  =#> udresult typeArchive "a new archive"
  #? T.unlines
     [ "Creates a new, empty archive."
     ]
  `since` initialVersion

typeArchive :: LuaError e => DocumentedType e Archive
typeArchive = deftype "Archive"
  []
  [ method tobinary
  ]

-- | Returns the raw binary string representation of the archive.
tobinary :: LuaError e => DocumentedFunction e
tobinary = defun "tobinary"
  ### liftPure Zip.fromArchive
  <#> udparam typeArchive "self" ""
  =#> functionResult pushLazyByteString "string" "bytes of the archive"
