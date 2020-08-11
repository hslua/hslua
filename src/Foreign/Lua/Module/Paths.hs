{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Module.Paths
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Lua module to work with file paths.
-}
module Foreign.Lua.Module.Paths (
  -- * Module
    pushModule
  , preloadModule
  , documentedModule

  -- * Path manipulations
  , drop_extensions
  , has_extension
  , is_absolute
  , is_relative
  , join_path
  , normalise
  , split_directories
  , take_directory
  , take_extensions
  , take_filename
  )
where

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#endif
import Data.Text (Text)
import Foreign.Lua (Lua, NumResults (..))
import Foreign.Lua.Call
import Foreign.Lua.Module hiding (preloadModule, pushModule)
import Foreign.Lua.Peek (Peeker, peekList, peekString)
import Foreign.Lua.Push (pushBool, pushList, pushString)

import qualified Data.Text as T
import qualified Foreign.Lua.Module as Module
import qualified System.FilePath as Path

--
-- Module
--

description :: Text
description = "Access to system information and functionality."

fields :: [Field]
fields = []

documentedModule :: Module
documentedModule = Module
  { moduleName = "paths"
  , moduleFields = fields
  , moduleDescription = description
  , moduleFunctions = functions
  }

-- | Pushes the @system@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = 1 <$ Module.pushModule documentedModule

-- | Add the @system@ module under the given name to the table of
-- preloaded packages.
preloadModule :: String -> Lua ()
preloadModule name = Module.preloadModule $
  documentedModule { moduleName = T.pack name }


--
-- Functions
--

functions :: [(Text, HaskellFunction)]
functions =
  [ ("drop_extensions", drop_extensions)
  , ("has_extension", has_extension)
  , ("is_absolute", is_absolute)
  , ("is_relative", is_relative)
  , ("join_path", join_path)
  , ("normalise", normalise)
  , ("split_directories", split_directories)
  , ("take_directory", take_directory)
  , ("take_extensions", take_extensions)
  , ("take_filename", take_filename)
  ]


-- | See @System.FilePath.dropExtension@
drop_extensions :: HaskellFunction
drop_extensions = toHsFnPrecursor Path.dropExtension
  <#> filepathParam
  =#> filepathResult "The modified filepath without extension"
  #? "Remove last extension, and the `.` preceding it."

-- | See @System.FilePath.hasExtension@
has_extension :: HaskellFunction
has_extension = toHsFnPrecursor Path.hasExtension
  <#> filepathParam
  =#> booleanResult ("`true` iff `filepath` has an extension, " <>
                     "`false` otherwise.")
  #? "Does the given filename has an extension?"

-- | See @System.FilePath.isAbsolute@
is_absolute :: HaskellFunction
is_absolute = toHsFnPrecursor Path.isAbsolute
  <#> filepathParam
  =#> booleanResult ("`true` iff `filepath` is an absolute path, " <>
                     "`false` otherwise.")
  #? "Checks whether a path is absolute, i.e. not fixed to a root."

-- | See @System.FilePath.isRelative@
is_relative :: HaskellFunction
is_relative = toHsFnPrecursor Path.isRelative
  <#> filepathParam
  =#> booleanResult ("`true` iff `filepath` is a relative path, " <>
                     "`false` otherwise.")
  #? "Checks whether a path is relative or fixed to a root."

-- | See @System.FilePath.joinPath@
join_path :: HaskellFunction
join_path = toHsFnPrecursor Path.joinPath
  <#> Parameter
      { parameterPeeker = peekList peekFilePath
      , parameterDoc = ParameterDoc
        { parameterName = "filepaths"
        , parameterType = "list of strings"
        , parameterDescription = "path components"
        , parameterIsOptional = False
        }
      }
  =#> filepathResult "The joined path."
  #? "Join path elements back together by the directory separator."

-- | See @System.FilePath.normalise@
normalise :: HaskellFunction
normalise = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> filepathResult "The normalised path."
  #? "Normalise a path."

-- | See @System.FilePath.splitDirectories@
split_directories :: HaskellFunction
split_directories = toHsFnPrecursor Path.splitDirectories
  <#> filepathParam
  =#> filepathListResult "A list of all directory paths."
  #? "Split a path by the directory separator."

-- | See @System.FilePath.takeDirectory@
take_directory :: HaskellFunction
take_directory = toHsFnPrecursor Path.normalise
  <#> filepathParam
  =#> filepathResult "The filepath up to the last directory separator."
  #? "Get the directory name; move up one level."

-- | See @System.FilePath.takeExtensions@
take_extensions :: HaskellFunction
take_extensions = toHsFnPrecursor Path.takeExtensions
  <#> filepathParam
  =#> filepathResult "String of all extensions."
  #? "Get all extensions."

-- | See @System.FilePath.takeFilename@
take_filename :: HaskellFunction
take_filename = toHsFnPrecursor Path.takeFileName
  <#> filepathParam
  =#> filepathResult "File name part of the input path."
  #? "Get the file name."

--
-- Parameters
--

peekFilePath :: Peeker FilePath
peekFilePath = peekString

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

filepathResult :: Text -- ^ Description
               -> FunctionResult FilePath
filepathResult desc = FunctionResult
  { fnResultPusher = \fp -> 1 <$ pushString fp
  , fnResultDoc = Just $ FunctionResultDoc
    { functionResultType = "string"
    , functionResultDescription = desc
    }
  }

filepathListResult :: Text -- ^ Description
                   -> FunctionResult [FilePath]
filepathListResult desc = FunctionResult
  { fnResultPusher = \fp -> 1 <$ pushList pushString fp
  , fnResultDoc = Just $ FunctionResultDoc
    { functionResultType = "list of strings"
    , functionResultDescription = desc
    }
  }

booleanResult :: Text -- ^ Description
              -> FunctionResult Bool
booleanResult desc = FunctionResult
  { fnResultPusher = \b -> 1 <$ pushBool b
  , fnResultDoc = Just $ FunctionResultDoc
    { functionResultType = "boolean"
    , functionResultDescription = desc
    }
  }
