{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * Zip archives
  , typeArchive
  , toarchive
  , create
  , entry_from_file
  -- ** archive methods
  , extract
  , tobinary
  -- * Archive entries
  , typeEntry
  , peekEntryFuzzy
  , contents
  )
where

import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Codec.Archive.Zip (Archive, Entry)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Version (Version, makeVersion)
import HsLua.Core
  ( LuaError, Type(..), failLua, liftIO, ltype )
import HsLua.Marshalling
  ( Peeker, failPeek, liftLua
  , peekFieldRaw, peekIntegral, peekLazyByteString, peekList, peekString
  , pushLazyByteString, pushList, pushString
  , retrieving, typeMismatchMessage )
import HsLua.Packaging

import qualified Codec.Archive.Zip as Zip
import qualified Data.Text as T

-- | The @zip@ module specification.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "zip"
  , moduleDescription = T.unwords
    [ "Function for creating, modifying, and extracting files from zip"
    , "archives."
    ]
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations = []
  }

-- | First published version of this library.
initialVersion :: Version
initialVersion = makeVersion [1,0,0]

--
-- Fields
--

-- | Exported fields.
fields :: [Field e]
fields = []


--
-- Functions
--

-- | Exported functions
functions :: LuaError e => [DocumentedFunction e]
functions =
  [ toarchive
  , create
  , entry_from_file
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
     [ "Reads an *Archive* structure from a raw zip archive; throws an error"
     , "if the given string cannot be decoded into an archive."
     ]
  `since` initialVersion

-- | Creates a new empty 'Archive'; wraps 'Zip.emptyArchive'.
create :: LuaError e => DocumentedFunction e
create = defun "create"
  ### (\case
          Nothing -> return Zip.emptyArchive
          Just fps -> liftIO $
            Zip.addFilesToArchive [] Zip.emptyArchive fps)
  <#> parameter (optional . peekList peekString) "{string,...}" "filepaths" ""
  =#> udresult typeArchive "a new archive"
  #? T.unlines
     [ "Creates a new, empty archive."
     ]
  `since` initialVersion

-- | Creates a new empty 'Archive'; wraps 'Zip.emptyArchive'.
entry_from_file :: LuaError e => DocumentedFunction e
entry_from_file = defun "entry_from_file"
  ### liftIO . Zip.readEntry []
  <#> parameter peekString "string" "filepath" ""
  =#> udresult typeEntry "a new zip archive entry"
  #? T.unlines
     [ "Generates a Entry from a file or directory."
     ]
  `since` initialVersion

--
-- * Archive
--

-- | The Lua 'Archive' type
typeArchive :: LuaError e => DocumentedType e Archive
typeArchive = deftype "ZipArchive"
  []
  [ property "entries" "files in this zip archive"
    (pushList (pushUD typeEntry), Zip.zEntries)
    (peekList peekEntryFuzzy, \ar entries -> ar { Zip.zEntries = entries })
  , method extract
  , method tobinary
  ]

-- | Returns the raw binary string representation of the archive;
-- wraps 'Zip.extractFilesFromArchive'
extract :: LuaError e => DocumentedFunction e
extract = defun "extract"
  ### (\archive -> liftIO $! Zip.extractFilesFromArchive [] archive)
  <#> udparam typeArchive "self" ""
  =#> []
  #? T.unlines
     [ "Extract all files from this archive, creating directories as needed."
     , "Note that the last-modified time is set correctly only in POSIX, not"
     , "in Windows. This function fails if encrypted entries are present."
     ]

-- | Returns the raw binary string representation of the archive.
tobinary :: LuaError e => DocumentedFunction e
tobinary = defun "tobinary"
  ### liftPure Zip.fromArchive
  <#> udparam typeArchive "self" ""
  =#> functionResult pushLazyByteString "string" "bytes of the archive"
  #? "Returns the raw binary string representation of the archive."

--
-- * Entry
--

-- | The Lua type for 'Entry' objects.
typeEntry :: LuaError e => DocumentedType e Entry
typeEntry = deftype "ZipEntry"
  []
  [ property "path" "relative path, using `/` as separator"
    (pushString, Zip.eRelativePath)
    (peekString, \entry path -> entry { Zip.eRelativePath = path })
  , method contents
  ]

-- | Returns the uncompressed contents of a zip entry.
contents :: LuaError e => DocumentedFunction e
contents = defun "contents"
  ### liftPure Zip.fromEntry
  <#> udparam typeEntry "self" ""
  =#> functionResult pushLazyByteString "string" "binary contents"

peekEntryFuzzy :: LuaError e => Peeker e Entry
peekEntryFuzzy = retrieving "ZipEntry" . \idx ->
  liftLua (ltype idx) >>= \case
    TypeUserdata -> peekUD typeEntry idx
    TypeTable    -> peekEntryFromTable idx
    _            -> failPeek =<<
                    typeMismatchMessage "ZipEntry userdata or table" idx

peekEntryFromTable :: LuaError e => Peeker e Entry
peekEntryFromTable idx = Zip.toEntry
  <$!> peekFieldRaw peekString "path" idx
  <*>  (peekFieldRaw (optional . peekIntegral) "modtime" idx >>= \case
          Nothing -> pure 0
          Just t  -> pure t)
  <*>  peekFieldRaw peekLazyByteString "contents" idx
