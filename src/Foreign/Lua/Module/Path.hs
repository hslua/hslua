{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Module.Path
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Lua module to work with file paths.
-}
module Foreign.Lua.Module.Path (
  -- * Module
    pushModule
  , preloadModule
  , documentedModule

  -- * Path manipulations
  , add_extension
  , combine
  , directory
  , filename
  , is_absolute
  , is_relative
  , join
  , normalize
  , split
  , split_extension
  , split_search_path
  , treat_strings_as_paths
  )
where

import Control.Monad (forM_)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Text (Text)
import Foreign.Lua
  ( Lua, NumResults (..), getglobal, getmetatable, nth, pop, rawset, remove
  , setfield, top )
import Foreign.Lua.Call
import Foreign.Lua.Module hiding (preloadModule, pushModule)
import Foreign.Lua.Peek (Peeker, peekList, peekString)
import Foreign.Lua.Push (pushBool, pushList, pushString, pushText)

import qualified Data.Text as T
import qualified Foreign.Lua.Module as Module
import qualified System.FilePath as Path

--
-- Module
--

description :: Text
description = "Module for file path manipulations."

documentedModule :: Module
documentedModule = Module
  { moduleName = "path"
  , moduleFields = fields
  , moduleDescription = description
  , moduleFunctions = functions
  }

-- | Pushes the @path@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = 1 <$ pushModule' documentedModule

-- | Add the @path@ module under the given name to the table of
-- preloaded packages.
preloadModule :: String -> Lua ()
preloadModule name = Module.preloadModule $
  documentedModule { moduleName = T.pack name }

-- | Helper function which pushes the module with its fields. This
-- function should be removed once the respective hslua bug has been
-- fixed.
pushModule' :: Module -> Lua ()
pushModule' mdl = do
  Module.pushModule mdl
  forM_ (moduleFields mdl) $ \field -> do
    pushText (fieldName field)
    fieldPushValue field
    rawset (nth 3)

--
-- Fields
--

-- | Exported fields.
fields :: [Field]
fields =
  [ separator
  , search_path_separator
  ]

-- | Wrapper for @'Path.pathSeparator'@.
separator :: Field
separator = Field
  { fieldName = "separator"
  , fieldDescription = "The character that separates directories."
  , fieldPushValue = pushString [Path.pathSeparator]
  }

-- | Wrapper for @'Path.searchPathSeparator'@.
search_path_separator :: Field
search_path_separator = Field
  { fieldName = "search_path_separator"
  , fieldDescription = "The character that is used to separate the entries in "
                    <> "the `PATH` environment variable."
  , fieldPushValue = pushString [Path.searchPathSeparator]
  }

--
-- Functions
--

functions :: [(Text, HaskellFunction)]
functions =
  [ ("directory", directory)
  , ("filename", filename)
  , ("is_absolute", is_absolute)
  , ("is_relative", is_relative)
  , ("join", join)
  , ("normalize", normalize)
  , ("split", split)
  , ("split_extension", split_extension)
  , ("split_search_path", split_search_path)
  , ("treat_strings_as_paths", treat_strings_as_paths)
  ]

-- | See @Path.takeDirectory@
directory :: HaskellFunction
directory = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> [filepathResult "The filepath up to the last directory separator."]
  #? "Get the directory name; move up one level."

-- | See @Path.takeFilename@
filename :: HaskellFunction
filename = toHsFnPrecursor Path.takeFileName
  <#> filepathParam
  =#> [filepathResult "File name part of the input path."]
  #? "Get the file name."

-- | See @Path.isAbsolute@
is_absolute :: HaskellFunction
is_absolute = toHsFnPrecursor Path.isAbsolute
  <#> filepathParam
  =#> [booleanResult ("`true` iff `filepath` is an absolute path, " <>
                      "`false` otherwise.")]
  #? "Checks whether a path is absolute, i.e. not fixed to a root."

-- | See @Path.isRelative@
is_relative :: HaskellFunction
is_relative = toHsFnPrecursor Path.isRelative
  <#> filepathParam
  =#> [booleanResult ("`true` iff `filepath` is a relative path, " <>
                      "`false` otherwise.")]
  #? "Checks whether a path is relative or fixed to a root."

-- | See @Path.joinPath@
join :: HaskellFunction
join = toHsFnPrecursor Path.joinPath
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

-- | See @Path.normalise@
normalize :: HaskellFunction
normalize = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> [filepathResult "The normalized path."]
  #? T.unlines
     [ "Normalizes a path."
     , ""
     , "- `//` outside of the drive can be made blank"
     , "- `/` becomes the `path.separator`"
     , "- `./` -> \'\'"
     , "- an empty path becomes `.`"
     ]

-- | See @Path.splitDirectories@.
--
-- Note that this does /not/ wrap @'Path.splitPath'@, as that function
-- adds trailing slashes to each directory, which is often inconvenient.
split :: HaskellFunction
split = toHsFnPrecursor Path.splitDirectories
  <#> filepathParam
  =#> [filepathListResult "List of all path components."]
  #? "Splits a path by the directory separator."

-- | See @Path.splitExtension@
split_extension :: HaskellFunction
split_extension = toHsFnPrecursor Path.splitExtension
  <#> filepathParam
  =#> [ FunctionResult
        { fnResultPusher = pushString . fst
        , fnResultDoc = FunctionResultDoc
          { functionResultType = "string"
          , functionResultDescription = "filepath without extension"
          }
        },
        FunctionResult
        { fnResultPusher = pushString . snd
        , fnResultDoc = FunctionResultDoc
          { functionResultType = "string"
          , functionResultDescription = "extension or empty string"
          }
        }
      ]
  #? ("Splits the last extension from a file path and returns the parts. "
      <> "The extension, if present, includes the leading separator; "
      <> "if the path has no extension, then the empty string is returned "
      <> "as the extension.")

-- | Wraps function @'Path.splitSearchPath'@.
split_search_path :: HaskellFunction
split_search_path = toHsFnPrecursor Path.splitSearchPath
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

-- | Join two paths with a directory separator. Wraps @'Path.combine'@.
combine :: HaskellFunction
combine = toHsFnPrecursor Path.combine
  <#> filepathParam
  <#> filepathParam
  =#> [filepathResult "combined paths"]
  #? "Combine two paths with a path separator."

-- | Adds an extension to a file path. Wraps @'Path.addExtension'@.
add_extension :: HaskellFunction
add_extension = toHsFnPrecursor Path.addExtension
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

treat_strings_as_paths :: HaskellFunction
treat_strings_as_paths = HaskellFunction
  { callFunction = do
      -- for some reason we can't just dump all functions into the
      -- string metatable, but have to use the string module for
      -- non-metamethods.
      pushString ""
      _ <- getmetatable top
      remove (nth 2)  -- dummy string
      pushHaskellFunction add_extension *> setfield (nth 2) "__add"
      pushHaskellFunction combine       *> setfield (nth 2) "__div"
      pop 1  -- string metatable

      getglobal "string"
      let pathFunctions =
            [ ("directory", directory)
            , ("filename", filename)
            , ("is_absolute", is_absolute)
            , ("is_relative", is_relative)
            , ("normalize", normalize)
            , ("split", split)
            , ("split_extension", split_extension)
            , ("split_search_path", split_search_path)
            ]
      forM_ pathFunctions $ \(key, fn) -> do
        pushHaskellFunction fn
        setfield (nth 2) key

      return (0 :: NumResults)
  , functionDoc = Nothing
  }
  #? "Augment the string module such that strings can be used as path objects."

--
-- Parameters
--

-- | Retrieves a file path from the stack.
peekFilePath :: Peeker FilePath
peekFilePath = peekString

-- | Filepath function parameter.
filepathParam :: Parameter FilePath
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
               -> FunctionResult FilePath
filepathResult desc = FunctionResult
  { fnResultPusher = \fp -> pushString fp
  , fnResultDoc = FunctionResultDoc
    { functionResultType = "string"
    , functionResultDescription = desc
    }
  }

-- | List of filepaths function result.
filepathListResult :: Text -- ^ Description
                   -> FunctionResult [FilePath]
filepathListResult desc = FunctionResult
  { fnResultPusher = \fp -> pushList pushString fp
  , fnResultDoc = FunctionResultDoc
    { functionResultType = "list of strings"
    , functionResultDescription = desc
    }
  }

-- | Boolean function result.
booleanResult :: Text -- ^ Description
              -> FunctionResult Bool
booleanResult desc = FunctionResult
  { fnResultPusher = \b -> pushBool b
  , fnResultDoc = FunctionResultDoc
    { functionResultType = "boolean"
    , functionResultDescription = desc
    }
  }
