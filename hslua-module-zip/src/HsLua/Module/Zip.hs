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
  , read_entry
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
import Codec.Archive.Zip (Archive, Entry, ZipOption (..), emptyArchive)
import Data.Maybe (catMaybes, fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Version (Version, makeVersion)
import HsLua.Core
  ( LuaError, Type(..), failLua, liftIO, ltype )
import HsLua.Marshalling
  ( Peeker, choice, failPeek, liftLua, peekBool
  , peekFieldRaw, peekIntegral, peekLazyByteString, peekList, peekString
  , pushLazyByteString, pushList, pushString
  , retrieving, typeMismatchMessage )
import HsLua.Packaging

import qualified Codec.Archive.Zip as Zip
import qualified Data.Text as T

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
#endif

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
  , read_entry
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

-- | Creates a new 'Archive'.
create :: LuaError e => DocumentedFunction e
create = defun "create"
  ### (\fpsOrEntries mopts ->
         let opts = fromMaybe [] mopts
         in case fpsOrEntries of
              Nothing ->
                return Zip.emptyArchive
              Just (Left filepaths) ->
                liftIO $! Zip.addFilesToArchive opts emptyArchive filepaths
              Just (Right entries)  ->
                return $! foldr Zip.addEntryToArchive emptyArchive entries)
  <#> opt (parameter (choice [ fmap Left  . peekList peekString
                             , fmap Right . peekList peekEntryFuzzy])
           "{string,...}|{ZipEntry,...}" "entries_or_filepaths" "")
  <#> opt (parameter peekZipOptions "table" "opts" "zip options")
  =#> udresult typeArchive "a new archive"
  #? T.unlines
     [ "Creates a new archive. If a list of ZipEntry objects is given, then a"
     , "new archive with just these entries is created. For a list of file"
     , "paths, this function reads these files and adds them to the"
     , "repository."
     ]
  `since` initialVersion

-- | Creates a new 'ZipEntry' from a file; wraps 'Zip.readEntry'.
read_entry :: LuaError e => DocumentedFunction e
read_entry = defun "read_entry"
  ### (\filepath mopts -> liftIO $! Zip.readEntry (fromMaybe [] mopts) filepath)
  <#> parameter peekString "string" "filepath" ""
  <#> opt (parameter peekZipOptions "table" "opts" "zipping options")
  =#> udresult typeEntry "a new zip archive entry"
  #? T.unlines
     [ "Generates a ZipEntry from a file or directory."
     ]
  `since` initialVersion

--
-- * Options
--
peekZipOptions :: LuaError e => Peeker e [ZipOption]
peekZipOptions = retrieving "Zip options" . \idx -> catMaybes <$> sequence
  [ optional (peekFieldRaw peekBool "recursive" idx) <&> \case
      Just True -> Just OptRecursive
      _         -> Nothing
  , optional (peekFieldRaw peekBool "verbose" idx) <&> \case
      Just True -> Just OptVerbose
      _         -> Nothing
  , optional (peekFieldRaw peekString "destination" idx) <&> \case
      Just fp -> Just (OptDestination fp)
      _       -> Nothing
  , optional (peekFieldRaw peekString "location" idx) <&> \case
      Just fp -> Just (OptLocation fp True)
      _       -> Nothing
  , optional (peekFieldRaw peekBool "preserve_symlinks" idx) <&> \case
      Just True -> (Just OptPreserveSymbolicLinks)
      _         -> Nothing
  ]

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
  ### (\entry mpasswd -> case mpasswd of
          Nothing -> return $! Zip.fromEntry entry
          Just passwd -> case Zip.fromEncryptedEntry passwd entry of
            Just contents' -> return $! contents'
            Nothing        -> failLua "Could not decrypt entry.")
  <#> udparam typeEntry "self" ""
  <#> opt (parameter peekString "string" "password" "password for entry")
  =#> functionResult pushLazyByteString "string" "binary contents"
  #? T.unlines
     [ "Get the uncompressed contents of a zip entry. If `password` is given,"
     , "then that password is used to decrypt the contents. An error is throws"
     , "if decrypting fails."
     ]

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
