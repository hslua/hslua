{-|
Module      : HsLua.Packaging.Types
Copyright   : © 2020-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>
Stability   : alpha
Portability : Portable

Marshaling and documenting Haskell functions.
-}
module HsLua.Packaging.Types
  ( -- * Documented module
    Module (..)
  , Field (..)
    -- * Documented functions
  , DocumentedFunction (..)
    -- ** Documentation types
  , FunctionDoc (..)
  , ParameterDoc (..)
  , ResultsDoc (..)
  , ResultValueDoc (..)
  , FieldDoc (..)
  -- * Type classes
  , HasName (..)
  , HasDescription (..)
  ) where

import Data.Text (Text)
import Data.Version (Version)
import HsLua.Core (LuaE, Name (fromName), NumResults)
import HsLua.ObjectOrientation (Operation)
import HsLua.Typing (TypeDocs, TypeSpec)
import qualified HsLua.Core.Utf8 as Utf8

-- | Named and documented Lua module.
data Module e = Module
  { moduleName :: Name
  , moduleDescription :: Text
  , moduleFields :: [Field e]
  , moduleFunctions :: [DocumentedFunction e]
  , moduleOperations :: [(Operation, DocumentedFunction e)]
  , moduleTypeInitializers :: [LuaE e Name]
  }

-- | Self-documenting module field
data Field e = Field
  { fieldName :: Name
  , fieldDoc :: FieldDoc
  , fieldPushValue :: LuaE e ()
  }

--
-- Function components
--

-- | Haskell equivallent to CFunction, i.e., function callable
-- from Lua.
data DocumentedFunction e = DocumentedFunction
  { callFunction :: LuaE e NumResults
  , functionName :: Name
  , functionDoc  :: FunctionDoc
  }

--
-- Documentation types
--

-- | Documentation for a Haskell function
data FunctionDoc = FunDoc
  { funDocName          :: Text
  , funDocDescription   :: Text
  , funDocParameters    :: [ParameterDoc]
  , funDocResults       :: ResultsDoc
  , funDocSince         :: Maybe Version  -- ^ Version in which the function
                                          -- was introduced.
  }
  deriving (Eq, Ord, Show)

-- | Documentation for function parameters.
data ParameterDoc = ParameterDoc
  { parameterName :: Text
  , parameterType :: TypeSpec
  , parameterDescription :: Text
  , parameterIsOptional :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Documentation for the return values of a function.
data ResultsDoc
  = ResultsDocList [ResultValueDoc]  -- ^ List of individual results
  | ResultsDocMult Text              -- ^ Flexible results
  deriving (Eq, Ord, Show)

-- | Documentation for a single return value of a function.
data ResultValueDoc = ResultValueDoc
  { resultValueType :: TypeSpec
  , resultValueDescription :: Text
  }
  deriving (Eq, Ord, Show)

-- | Documentation for a module field.
data FieldDoc = FieldDoc
  { fieldDocName :: Text
  , fieldDocType :: TypeSpec
  , fieldDocDescription :: Text
  }

--
-- Type Classes
--

-- | Objects that have descriptions.
class HasDescription a where
  getDescription :: a -> Text
  setDescription :: a -> Text -> a

instance HasDescription FieldDoc where
  getDescription = fieldDocDescription
  setDescription fd descr = fd { fieldDocDescription = descr }

instance HasDescription (Module e) where
  getDescription = moduleDescription
  setDescription mdl descr = mdl { moduleDescription = descr }

instance HasDescription (Field e) where
  getDescription = fieldDocDescription . fieldDoc
  setDescription fld descr =
    let doc = fieldDoc fld
    in fld { fieldDoc = setDescription doc descr }

-- | Named objects
class HasName a where
  getName :: a -> Name
  setName :: a -> Name -> a

instance HasName (Field e) where
  getName = fieldName
  setName fd name =
    let doc = fieldDoc fd
    in fd
    { fieldName = name
    , fieldDoc = doc { fieldDocName = Utf8.toText $ fromName name }
    }

instance HasName (Module e) where
  getName = moduleName
  setName mdl name = mdl { moduleName = name }

instance HasName (DocumentedFunction e) where
  getName = functionName
  setName fn name =
    let fnDoc = functionDoc fn
    in fn
       { functionName = name
       , functionDoc = fnDoc { funDocName = Utf8.toText $ fromName name }
       }
