{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.Module.Zip
Copyright   : © 2022-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Lua module to work with file zips.
-}
module HsLua.Module.Zip (
  -- * Module
    documentedModule

  -- * Zip archives
  , typeArchive
  , mkArchive
  , read_entry
  , zip
  -- ** archive methods
  , extract
  , bytestring
  -- * Zip entry
  , typeEntry
  , peekEntryFuzzy
  -- ** entry methods
  , contents
  -- * Zip Options
  , peekZipOptions
  )
where

import Prelude hiding (zip)
import Control.Applicative (optional)
import Control.Monad ((<$!>))
import Codec.Archive.Zip (Archive, Entry, ZipOption (..), emptyArchive)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (Version, makeVersion)
import HsLua.Core
  ( LuaError, NumArgs (..), NumResults (..), Type(..), call, failLua
  , fromStackIndex, getfield, gettop, replace, liftIO, ltype
  , nth, nthBottom, setmetatable )
import HsLua.List (newListMetatable)
import HsLua.Marshalling
  ( Peeker, Pusher, choice, failPeek, liftLua, peekBool
  , peekFieldRaw, peekIntegral, peekLazyByteString, peekList, peekString
  , pushLazyByteString, pushList, pushIntegral, pushString
  , retrieving, typeMismatchMessage )
import HsLua.Packaging

import qualified Codec.Archive.Zip as Zip
import qualified Data.Text as T


-- | The @zip@ module specification.
documentedModule :: forall e. LuaError e => Module e
documentedModule = Module
  { moduleName = "zip"
  , moduleDescription = T.unlines
    [ "Functions to create, modify, and extract files from zip archives."
    , ""
    , "The module can be called as a function, in which case it behaves"
    , "like the `zip` function described below."
    , ""
    , "Zip options are optional; when defined, they must be a table with"
    , "any of the following keys:"
    , ""
    , "  - `recursive`: recurse directories when set to `true`;"
    , "  - `verbose`: print info messages to stdout;"
    , "  - `destination`: the value specifies the directory in which to"
    , "    extract;"
    , "  - `location`: value is used as path name, defining where files"
    , "    are placed."
    , "  - `preserve_symlinks`: Boolean value, controlling whether"
    , "    symbolic links are preserved as such. This option is ignored"
    , "    on Windows."
    ]
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations =
    [ operation Call $ lambda
      ### (do
              -- call function `zip`
              _ <- getfield (nthBottom 1) (functionName @e zip)
              replace (nthBottom 1)
              nargs <- NumArgs . subtract 1 . fromStackIndex <$> gettop
              call nargs 1
              pure (NumResults 1))
      =?> "new Archive"
    ]
  , moduleTypeInitializers =
      [ initType typeArchive
      , initType typeEntry
      ]
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
  [ mkArchive
  , mkEntry
  , read_entry
  , zip
  ]

-- | Creates a new 'Archive' from a list of files.
zip :: LuaError e => DocumentedFunction e
zip = defun "zip"
  ### (\filepaths mopts ->
         let opts = fromMaybe [] mopts
         in liftIO $! Zip.addFilesToArchive opts emptyArchive filepaths)
  <#> parameter (peekList peekString) "{string,...}"
       "filepaths" "list of files from which the archive is created."
  <#> opt (parameter peekZipOptions "table" "opts" "zip options")
  =#> udresult typeArchive "a new archive"
  #? T.unlines
     [ "Package and compress the given files into a new Archive." ]
  `since` initialVersion

-- | Creates a new 'ZipEntry' from a file; wraps 'Zip.readEntry'.
read_entry :: LuaError e => DocumentedFunction e
read_entry = defun "read_entry"
  ### (\filepath mopts -> liftIO $! Zip.readEntry (fromMaybe [] mopts) filepath)
  <#> parameter peekString "string" "filepath" ""
  <#> opt (parameter peekZipOptions "table" "opts" "zip options")
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
    (pushEntries, Zip.zEntries)
    (peekList peekEntryFuzzy, \ar entries -> ar { Zip.zEntries = entries })
  , method extract
  , method bytestring
  ]

-- | Wrapper for 'Zip.toArchive'; converts a string into an Archive.
mkArchive :: LuaError e => DocumentedFunction e
mkArchive = defun "Archive"
  ### (\case
          Nothing                ->
            pure Zip.emptyArchive
          Just (Left bytestring') ->
            either failLua pure $ Zip.toArchiveOrFail bytestring'
          Just (Right entries)   ->
            pure $ foldr Zip.addEntryToArchive emptyArchive entries)
  <#> opt (parameter (choice [ fmap Left  . peekLazyByteString
                             , fmap Right . peekList peekEntryFuzzy ])
           "string|{ZipEntry,...}" "bytestring_or_entries"
           "binary archive data or list of entries; defaults to an empty list")
  =#> udresult typeArchive "new Archive"
  #? T.unlines
     [ "Reads an *Archive* structure from a raw zip archive or a list of"
     , "Entry items; throws an error if the given string cannot be decoded"
     , "into an archive."
     ]
  `since` initialVersion

-- | Returns the raw binary string representation of the archive;
-- wraps 'Zip.extractFilesFromArchive'
extract :: LuaError e => DocumentedFunction e
extract = defun "extract"
  ### (\archive mopts ->
         liftIO $! Zip.extractFilesFromArchive (fromMaybe [] mopts) archive)
  <#> udparam typeArchive "self" ""
  <#> opt (parameter peekZipOptions "table" "opts" "zip options")
  =#> []
  #? T.unlines
     [ "Extract all files from this archive, creating directories as needed."
     , "Note that the last-modified time is set correctly only in POSIX, not"
     , "in Windows. This function fails if encrypted entries are present."
     ]

-- | Returns the raw binary string representation of the archive.
bytestring :: LuaError e => DocumentedFunction e
bytestring = defun "bytestring"
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
  , property "modtime" "modification time (seconds since unix epoch)"
    (pushIntegral, Zip.eLastModified)
    (peekIntegral, \entry modtime -> entry { Zip.eLastModified = modtime})
  , method contents
  ]

-- | Creates a new 'ZipEntry' from a file; wraps 'Zip.readEntry'.
mkEntry :: LuaError e => DocumentedFunction e
mkEntry = defun "Entry"
  ### (\filepath contents' mmodtime -> do
          modtime <- maybe (floor <$> liftIO getPOSIXTime) pure mmodtime
          pure $ Zip.toEntry filepath modtime contents')
  <#> parameter peekString "string" "path" "file path in archive"
  <#> parameter peekLazyByteString "string" "contents" "uncompressed contents"
  <#> opt (parameter peekIntegral "integer" "modtime" "modification time")
  =#> udresult typeEntry "a new zip archive entry"
  #? T.unlines
     [ "Generates a ZipEntry from a filepath, uncompressed content, and"
     , "the file's modification time."
     ]
  `since` initialVersion

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

-- | Pushes a list of entries as an Entries object, i.e., a list with
-- additional methods.
pushEntries :: LuaError e => Pusher e [Entry]
pushEntries es = do
  pushList (pushUD typeEntry) es
  newListMetatable "ZipEntry list" (pure ())
  setmetatable (nth 2)
