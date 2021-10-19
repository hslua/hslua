{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Module.Path
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Lua module to work with file paths.
-}
module HsLua.Module.Path (
  -- * Module
    documentedModule

  -- * Fields
  , separator
  , search_path_separator

  -- * Path manipulation
  , add_extension
  , combine
  , directory
  , filename
  , is_absolute
  , is_relative
  , join
  , make_relative
  , normalize
  , split
  , split_extension
  , split_search_path
  , treat_strings_as_paths
  )
where

import Data.Char (toLower)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Text (Text)
import Data.Version (Version, makeVersion)
import HsLua.Core
  ( LuaError, getglobal, getmetatable, nth, pop, rawset, remove, top )
import HsLua.Marshalling
  ( Peeker, peekBool, peekList, peekString
  , pushBool, pushList, pushName, pushString )
import HsLua.Packaging

import qualified Data.Text as T
import qualified System.FilePath as Path

-- | The @path@ module specification.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "path"
  , moduleDescription = "Module for file path manipulations."
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations = []
  }

--
-- Fields
--

-- | Exported fields.
fields :: [Field e]
fields =
  [ separator
  , search_path_separator
  ]

-- | Wrapper for @'Path.pathSeparator'@.
separator :: Field e
separator = Field
  { fieldName = "separator"
  , fieldDescription = "The character that separates directories."
  , fieldPushValue = pushString [Path.pathSeparator]
  }

-- | Wrapper for @'Path.searchPathSeparator'@.
search_path_separator :: Field e
search_path_separator = Field
  { fieldName = "search_path_separator"
  , fieldDescription = "The character that is used to separate the entries in "
                    <> "the `PATH` environment variable."
  , fieldPushValue = pushString [Path.searchPathSeparator]
  }

--
-- Functions
--

functions :: LuaError e => [DocumentedFunction e]
functions =
  [ directory
  , filename
  , is_absolute
  , is_relative
  , join
  , make_relative
  , normalize
  , split
  , split_extension
  , split_search_path
  , treat_strings_as_paths
  ]

-- | See @Path.takeDirectory@
directory :: DocumentedFunction e
directory = defun "directory"
  ### liftPure Path.takeDirectory
  <#> filepathParam
  =#> [filepathResult "The filepath up to the last directory separator."]
  #? ("Gets the directory name, i.e., removes the last directory " <>
      "separator and everything after from the given path.")
  `since` initialVersion

-- | See @Path.takeFilename@
filename :: DocumentedFunction e
filename = defun "filename"
  ### liftPure Path.takeFileName
  <#> filepathParam
  =#> [filepathResult "File name part of the input path."]
  #? "Get the file name."
  `since` initialVersion

-- | See @Path.isAbsolute@
is_absolute :: DocumentedFunction e
is_absolute = defun "is_absolute"
  ### liftPure Path.isAbsolute
  <#> filepathParam
  =#> [booleanResult ("`true` iff `filepath` is an absolute path, " <>
                      "`false` otherwise.")]
  #? "Checks whether a path is absolute, i.e. not fixed to a root."
  `since` initialVersion

-- | See @Path.isRelative@
is_relative :: DocumentedFunction e
is_relative = defun "is_relative"
  ### liftPure Path.isRelative
  <#> filepathParam
  =#> [booleanResult ("`true` iff `filepath` is a relative path, " <>
                      "`false` otherwise.")]
  #? "Checks whether a path is relative or fixed to a root."
  `since` initialVersion

-- | See @Path.joinPath@
join :: LuaError e => DocumentedFunction e
join = defun "join"
  ### liftPure Path.joinPath
  <#> Parameter
      { parameterPeeker = peekList peekFilePath
      , parameterDoc = ParameterDoc
        { parameterName = "filepaths"
        , parameterType = "list of strings"
        , parameterDescription = "path components"
        , parameterIsOptional = False
        }
      }
  =#> [filepathResult "The joined path."]
  #? "Join path elements back together by the directory separator."
  `since` initialVersion

make_relative :: DocumentedFunction e
make_relative = defun "make_relative"
  ### liftPure3 makeRelative
  <#> parameter
        peekFilePath
        "string"
        "path"
        "path to be made relative"
  <#> parameter
        peekFilePath
        "string"
        "root"
        "root path"
  <#> optionalParameter
        peekBool
        "boolean"
        "unsafe"
        "whether to allow `..` in the result."
  =#> [filepathResult "contracted filename"]
  #? mconcat
     [ "Contract a filename, based on a relative path. Note that the "
     , "resulting path will never introduce `..` paths, as the "
     , "presence of symlinks means `../b` may not reach `a/b` if it "
     , "starts from `a/c`. For a worked example see "
     , "[this blog post](http://neilmitchell.blogspot.co.uk"
     , "/2015/10/filepaths-are-subtle-symlinks-are-hard.html)."
     ]
  `since` initialVersion

-- | See @Path.normalise@
normalize :: DocumentedFunction e
normalize = defun "normalize"
  ### liftPure Path.normalise
  <#> filepathParam
  =#> [filepathResult "The normalized path."]
  #? T.unlines
     [ "Normalizes a path."
     , ""
     , " - `//` makes sense only as part of a (Windows) network drive;"
     , "   elsewhere, multiple slashes are reduced to a single"
     , "   `path.separator` (platform dependent)."
     , " - `/` becomes `path.separator` (platform dependent)."
     , " - `./` is removed."
     , " - an empty path becomes `.`"
     ]
  `since` initialVersion

-- | See @Path.splitDirectories@.
--
-- Note that this does /not/ wrap @'Path.splitPath'@, as that function
-- adds trailing slashes to each directory, which is often inconvenient.
split :: LuaError e => DocumentedFunction e
split = defun "split"
  ### liftPure Path.splitDirectories
  <#> filepathParam
  =#> [filepathListResult "List of all path components."]
  #? "Splits a path by the directory separator."
  `since` initialVersion

-- | See @Path.splitExtension@
split_extension :: DocumentedFunction e
split_extension = defun "split_extension"
  ### liftPure Path.splitExtension
  <#> filepathParam
  =#> [ FunctionResult
        { fnResultPusher = pushString . fst
        , fnResultDoc = ResultValueDoc
          { resultValueType = "string"
          , resultValueDescription = "filepath without extension"
          }
        },
        FunctionResult
        { fnResultPusher = pushString . snd
        , fnResultDoc = ResultValueDoc
          { resultValueType = "string"
          , resultValueDescription = "extension or empty string"
          }
        }
      ]
  #? ("Splits the last extension from a file path and returns the parts. "
      <> "The extension, if present, includes the leading separator; "
      <> "if the path has no extension, then the empty string is returned "
      <> "as the extension.")
  `since` initialVersion

-- | Wraps function @'Path.splitSearchPath'@.
split_search_path :: LuaError e => DocumentedFunction e
split_search_path = defun "split_search_path"
  ### liftPure Path.splitSearchPath
  <#> Parameter
      { parameterPeeker = peekString
      , parameterDoc = ParameterDoc
        { parameterName = "search_path"
        , parameterType = "string"
        , parameterDescription = "platform-specific search path"
        , parameterIsOptional = False
        }
      }
  =#> [filepathListResult "list of directories in search path"]
  #? ("Takes a string and splits it on the `search_path_separator` "
      <> "character. Blank items are ignored on Windows, "
      <> "and converted to `.` on Posix. "
      <> "On Windows path elements are stripped of quotes.")
  `since` initialVersion

-- | Join two paths with a directory separator. Wraps @'Path.combine'@.
combine :: DocumentedFunction e
combine = defun "combine"
  ### liftPure2 Path.combine
  <#> filepathParam
  <#> filepathParam
  =#> [filepathResult "combined paths"]
  #? "Combine two paths with a path separator."

-- | Adds an extension to a file path. Wraps @'Path.addExtension'@.
add_extension :: DocumentedFunction e
add_extension = defun "add_extension"
  ### liftPure2 Path.addExtension
  <#> filepathParam
  <#> Parameter
      { parameterPeeker = peekString
      , parameterDoc = ParameterDoc
        { parameterName = "extension"
        , parameterType = "string"
        , parameterDescription = "an extension, with or without separator dot"
        , parameterIsOptional = False
        }
      }
  =#> [filepathResult "filepath with extension"]
  #? "Adds an extension, even if there is already one."
  `since` initialVersion

stringAugmentationFunctions :: LuaError e => [DocumentedFunction e]
stringAugmentationFunctions =
  [ directory
  , filename
  , is_absolute
  , is_relative
  , normalize
  , split
  , split_extension
  , split_search_path
  ]

treat_strings_as_paths :: LuaError e => DocumentedFunction e
treat_strings_as_paths = defun "treat_strings_as_paths"
  ### do let addFunction fn = do
                 pushName (functionName fn)
                 pushDocumentedFunction fn
                 rawset (nth 3)
         -- for some reason we can't just dump all functions into the
         -- string metatable, but have to use the string module for
         -- non-metamethods.
         pushString "" *> getmetatable top *> remove (nth 2)
         mapM_ addFunction
           [setName "__add" add_extension, setName "__div" combine]
         pop 1  -- string metatable

         _ <- getglobal "string"
         mapM_ addFunction stringAugmentationFunctions
         pop 1 -- string module
  =#> []
  #? ("Augment the string module such that strings can be used as "
      <> "path objects.")
  `since` initialVersion

--
-- Parameters
--

-- | Retrieves a file path from the stack.
peekFilePath :: Peeker e FilePath
peekFilePath = peekString

-- | Filepath function parameter.
filepathParam :: Parameter e FilePath
filepathParam = Parameter
  { parameterPeeker = peekFilePath
  , parameterDoc = ParameterDoc
    { parameterName = "filepath"
    , parameterType = "string"
    , parameterDescription = "path"
    , parameterIsOptional = False
    }
  }

-- | Result of a function returning a file path.
filepathResult :: Text -- ^ Description
               -> FunctionResult e FilePath
filepathResult desc = FunctionResult
  { fnResultPusher = pushString
  , fnResultDoc = ResultValueDoc
    { resultValueType = "string"
    , resultValueDescription = desc
    }
  }

-- | List of filepaths function result.
filepathListResult :: LuaError e
                   => Text -- ^ Description
                   -> FunctionResult e [FilePath]
filepathListResult desc = FunctionResult
  { fnResultPusher = pushList pushString
  , fnResultDoc = ResultValueDoc
    { resultValueType = "list of strings"
    , resultValueDescription = desc
    }
  }

-- | Boolean function result.
booleanResult :: Text -- ^ Description
              -> FunctionResult e Bool
booleanResult desc = FunctionResult
  { fnResultPusher = pushBool
  , fnResultDoc = ResultValueDoc
    { resultValueType = "boolean"
    , resultValueDescription = desc
    }
  }

--
-- Helpers
--

-- | Alternative version of @'Path.makeRelative'@, which introduces @..@
-- paths if desired.
makeRelative :: FilePath      -- ^ path to be made relative
             -> FilePath      -- ^ root directory from which to start
             -> Maybe Bool    -- ^ whether to use unsafe relative paths.
             -> FilePath
makeRelative path root unsafe
 | Path.equalFilePath root path = "."
 | takeAbs root /= takeAbs path = path
 | otherwise = go (dropAbs path) (dropAbs root)
  where
    go x "" = dropWhile Path.isPathSeparator x
    go x y =
      let (x1, x2) = breakPath x
          (y1, y2) = breakPath y
      in case () of
        _ | Path.equalFilePath x1 y1 -> go x2 y2
        _ | unsafe == Just True      -> Path.joinPath ["..", x1, go x2 y2]
        _                            -> path

    breakPath = both (dropWhile Path.isPathSeparator)
              . break Path.isPathSeparator
              . dropWhile Path.isPathSeparator

    both f (a, b) = (f a, f b)

    leadingPathSepOnWindows = \case
      ""                  -> False
      x | Path.hasDrive x -> False
      c:_                 -> Path.isPathSeparator c

    dropAbs x = if leadingPathSepOnWindows x then tail x else Path.dropDrive x

    takeAbs x = if leadingPathSepOnWindows x
                then [Path.pathSeparator]
                else map (\y ->
                            if Path.isPathSeparator y
                            then Path.pathSeparator
                            else toLower y)
                         (Path.takeDrive x)

-- | First published version of this library.
initialVersion :: Version
initialVersion = makeVersion [0,1,0]
