{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-|
Module      : HsLua.Typing
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

The module provides Haskell types and values that can be used to
describe and declare the types of Lua values.
-}
module HsLua.Typing
  ( TypeSpec (..)
  , TypeDocs (..)
  , (#|#)
  , typeSpecToString
    -- * Types
  , anyType
  , voidType
    -- ** Built-in types
  , booleanType
  , functionType
  , integerType
  , lightUserdataType
  , nilType
  , numberType
  , stringType
  , tableType
  , threadType
  , userdataType
    -- ** Type constructors
  , seqType
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import HsLua.Core (Name (fromName))
import HsLua.Core.Utf8 (toString)
import qualified HsLua.Core as HsLua
import qualified Data.Map as Map

-- | Type specification for Lua values.
data TypeSpec =
    AnyType                             -- ^ Unconstrained type.
  | BasicType HsLua.Type                -- ^ Built-in type
  | NamedType TypeDocs                  -- ^ A type that's been given a name.
  | SequenceType TypeSpec               -- ^ Sequence of the given type.
  | SumType [TypeSpec]                  -- ^ Union type; a sum type.
  | RecordType (Map.Map Name TypeSpec)  -- ^ Record type (type product).
  | FunctionType [TypeSpec] [TypeSpec]  -- ^ Function type.
  deriving (Eq, Generic, Ord, Show)

-- | Documented custom type.
data TypeDocs = TypeDocs
  { typeName        :: Name
  , typeDescription :: Text
  , typeSpec        :: TypeSpec
  , typeRegistry    :: Maybe Name
  }
  deriving (Eq, Generic, Ord, Show)

-- | Returns the sum of two type specifiers, declaring that a Lua value
-- can have either type.
(#|#) :: TypeSpec -> TypeSpec -> TypeSpec
SumType a #|# SumType b = SumType (a ++ b)
_         #|# _         = AnyType

-- | Generate a string representation of the type specifier.
typeSpecToString :: TypeSpec -> String
typeSpecToString = \case
  AnyType        -> "any"
  FunctionType{} -> "function"
  RecordType{}   -> "table"
  BasicType t    -> map toLower . drop 4 $ show t
  NamedType nt   -> toString . fromName $ typeName nt
  SequenceType t -> '{' : typeSpecToString t ++ ",...}"
  SumType specs  -> intercalate "|" (map typeSpecToString specs)

--
-- Built-in types
--

-- | Unconstraint type; any Lua value.
anyType :: TypeSpec
anyType = AnyType

-- | A type for which there cannot be any value.
voidType :: TypeSpec
voidType = SumType []

-- | The built-in @boolean@ Lua type.
booleanType :: TypeSpec
booleanType = BasicType HsLua.TypeBoolean

-- | The built-in @function@ Lua type.
functionType :: TypeSpec
functionType = BasicType HsLua.TypeFunction

-- | The built-in @number@ Lua type.
integerType :: TypeSpec
integerType = BasicType HsLua.TypeNumber

-- | The built-in @light userdata@ Lua type.
lightUserdataType :: TypeSpec
lightUserdataType = BasicType HsLua.TypeLightUserdata

-- | The built-in @nil@ Lua type.
nilType :: TypeSpec
nilType = BasicType HsLua.TypeNil

-- | The built-in @number@ Lua type.
numberType :: TypeSpec
numberType = BasicType HsLua.TypeNumber

-- | The built-in @string@ Lua type.
stringType :: TypeSpec
stringType = BasicType HsLua.TypeString

-- | The built-in @table@ Lua type.
tableType :: TypeSpec
tableType = BasicType HsLua.TypeTable

-- | The built-in @thread@ Lua type.
threadType :: TypeSpec
threadType = BasicType HsLua.TypeThread

-- | The built-in @userdata@ Lua type.
userdataType :: TypeSpec
userdataType = BasicType HsLua.TypeUserdata

-- | For backwards compatibility and convenience, strings can be used as
-- TypeSpec values.
instance IsString TypeSpec where
  fromString = \case
    "any"            -> anyType
    "boolean"        -> booleanType
    "function"       -> functionType
    "integer"        -> numberType
    "light userdata" -> lightUserdataType
    "nil"            -> nilType
    "number"         -> numberType
    "string"         -> stringType
    "table"          -> tableType
    "userdata"       -> userdataType
    s -> NamedType $ TypeDocs
         { typeName = fromString s
         , typeSpec = anyType
         , typeDescription = mempty
         , typeRegistry = Nothing
         }

--
-- Constructors
--

seqType :: TypeSpec -> TypeSpec
seqType = SequenceType
