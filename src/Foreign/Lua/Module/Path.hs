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
  , drop_extensions
  , has_extension
  , is_absolute
  , is_relative
  , join
  , normalise
  , split_path
  , split_search_path
  , take_directory
  , take_extensions
  , take_filename
  )
where

import Control.Monad (forM_)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Text (Text)
import Foreign.Lua (Lua, NumResults (..), nth, rawset)
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
  [ ("drop_extensions", drop_extensions)
  , ("has_extension", has_extension)
  , ("is_absolute", is_absolute)
  , ("is_relative", is_relative)
  , ("join", join)
  , ("normalise", normalise)
  , ("split_path", split_path)
  , ("split_search_path", split_search_path)
  , ("take_directory", take_directory)
  , ("take_extensions", take_extensions)
  , ("take_filename", take_filename)
  ]


-- | See @Path.dropExtension@
drop_extensions :: HaskellFunction
drop_extensions = toHsFnPrecursor Path.dropExtension
  <#> filepathParam
  =#> [filepathResult "The modified filepath without extension"]
  #? "Remove last extension, and the `.` preceding it."

-- | See @Path.hasExtension@
has_extension :: HaskellFunction
has_extension = toHsFnPrecursor Path.hasExtension
  <#> filepathParam
  =#> [booleanResult ("`true` iff `filepath` has an extension, " <>
                      "`false` otherwise.")]
  #? "Does the given filename has an extension?"

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
normalise :: HaskellFunction
normalise = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> [filepathResult "The normalised path."]
  #? "Normalise a path."

-- | See @Path.splitDirectories@.
--
-- Note that this does /not/ wrap @'Path.splitPath'@, as that function
-- adds trailing slashes to each directory, which is often inconvenient.
split_path :: HaskellFunction
split_path = toHsFnPrecursor Path.splitDirectories
  <#> filepathParam
  =#> [filepathListResult "List of all path components."]
  #? "Split a path by the directory separator."

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

-- | See @Path.takeDirectory@
take_directory :: HaskellFunction
take_directory = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> [filepathResult "The filepath up to the last directory separator."]
  #? "Get the directory name; move up one level."

-- | See @Path.takeExtensions@
take_extensions :: HaskellFunction
take_extensions = toHsFnPrecursor Path.takeExtensions
  <#> filepathParam
  =#> [filepathResult "String of all extensions."]
  #? "Get all extensions."

-- | See @Path.takeFilename@
take_filename :: HaskellFunction
take_filename = toHsFnPrecursor Path.takeFileName
  <#> filepathParam
  =#> [filepathResult "File name part of the input path."]
  #? "Get the file name."

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
