{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , recType
  , seqType
    -- * Marshalling
  , pushTypeSpec
  , peekTypeSpec
  , pushTypeDoc
  , peekTypeDoc
  ) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import HsLua.Core
import HsLua.Core.Utf8 (toString)
import HsLua.Marshalling
import qualified HsLua.Core as HsLua
import qualified Data.Map as Map

-- | Type specification for Lua values.
data TypeSpec =
    BasicType HsLua.Type              -- ^ Built-in type
  | NamedType Name                    -- ^ A type that's been given a name.
  | SeqType TypeSpec                  -- ^ Sequence of the given type.
  | SumType [TypeSpec]                -- ^ Union type; a sum type.
  | RecType (Map.Map Name TypeSpec)   -- ^ Record type (type product).
  | FunType [TypeSpec] [TypeSpec]     -- ^ Function type.
  | AnyType                           -- ^ Unconstrained type.
  deriving (Eq, Generic, Ord, Show)

-- | Documented custom type.
data TypeDocs = TypeDocs
  { typeDescription :: Text
  , typeSpec        :: TypeSpec
  , typeRegistry    :: Maybe Name
  }
  deriving (Eq, Generic, Ord, Show)

-- | Returns the sum of two type specifiers, declaring that a Lua value
-- can have either type.
(#|#) :: TypeSpec -> TypeSpec -> TypeSpec
AnyType    #|# _          = AnyType
_          #|# AnyType    = AnyType
SumType [] #|# b          = b                   -- `SumType []` is `Void`
a          #|# SumType [] = a
SumType a  #|# SumType b  = SumType (a ++ b)
SumType a  #|# b          = SumType (a ++ [b])
a          #|# SumType b  = SumType (a : b)
a          #|# b          =
  if a == b
  then a
  else SumType [a, b]

-- | Generate a string representation of the type specifier.
typeSpecToString :: TypeSpec -> String
typeSpecToString = \case
  BasicType t   -> map toLower . drop 4 $ show t
  NamedType nt  -> toString $ fromName nt
  AnyType       -> "any"
  FunType{}     -> "function"
  RecType{}     -> "table"
  SeqType t     -> '{' : typeSpecToString t ++ ",...}"
  SumType specs -> intercalate "|" (map typeSpecToString specs)

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

-- | A Lua integer type.
integerType :: TypeSpec
integerType = NamedType "integer"

-- | For backwards compatibility and convenience, strings can be used as
-- TypeSpec values.
instance IsString TypeSpec where
  fromString = \case
    "any"            -> anyType
    "boolean"        -> booleanType
    "function"       -> functionType
    "integer"        -> integerType
    "light userdata" -> lightUserdataType
    "nil"            -> nilType
    "number"         -> numberType
    "string"         -> stringType
    "table"          -> tableType
    "userdata"       -> userdataType
    s                -> NamedType (fromString s)

--
-- Constructors
--

-- | Creates a sequence type.
seqType :: TypeSpec -> TypeSpec
seqType = SeqType

-- | Creates a record type.
recType :: [(Name, TypeSpec)] -> TypeSpec
recType = RecType . Map.fromList

--
-- Marshalling
--

-- | Pushes documentation for a custom type.
pushTypeDoc :: LuaError e => Pusher e TypeDocs
pushTypeDoc = pushAsTable
  [ ("description", pushText . typeDescription)
  , ("typespec", pushTypeSpec . typeSpec)
  , ("registry", maybe pushnil pushName . typeRegistry)
  ]

-- | Retrieves a custom type specifier.
peekTypeDoc :: LuaError e => Peeker e TypeDocs
peekTypeDoc = typeChecked "TypeDoc" istable $ \idx -> do
  desc <- peekFieldRaw peekText "description" idx
  spec <- peekFieldRaw peekTypeSpec "typespec" idx
  regn <- peekFieldRaw (peekNilOr peekName) "registry" idx
  return $ TypeDocs desc spec regn

-- | Pushes a table representation of a 'TypeSpec' to the stack.
pushTypeSpec :: LuaError e
             => TypeSpec
             -> LuaE e ()
pushTypeSpec ts = do
  checkstack' 4 "HsLua.Typing.pushTypeSpec"
  case ts of
    BasicType bt  -> pushAsTable [("basic", pushString . show)] bt
    NamedType n   -> pushAsTable [("named", pushName)] n
    SeqType seq'  -> pushAsTable [("sequence", pushTypeSpec)] seq'
    SumType st    -> pushAsTable [("sum", pushList pushTypeSpec)] st
    RecType rt    -> pushAsTable [("record", pushMap pushName pushTypeSpec)] rt
    FunType dt ct -> pushAsTable [("domain", pushList pushTypeSpec . fst)
                                 ,("codomain", pushList pushTypeSpec . snd)]
                                 (dt, ct)
    AnyType       -> pushAsTable [("any", pushBool)] True
  created <- newmetatable "HsLua.TypeSpec"
  when created $ do
    pushHaskellFunction $ do
      ts' <- forcePeek $ peekTypeSpec (nth 1)
      pushString $ typeSpecToString ts'
      return 1
    setfield (nth 2) "__tostring"
  setmetatable (nth 2)

-- | Retrieves a 'TypeSpec' from a table on the stack.
peekTypeSpec :: LuaError e => Peeker e TypeSpec
peekTypeSpec = typeChecked "TypeSpec" istable $ \idx ->
  choice
  [ fmap BasicType . peekFieldRaw peekRead "basic"
  , fmap NamedType . peekFieldRaw peekName "named"
  , fmap SeqType . peekFieldRaw peekTypeSpec "sequence"
  , fmap SumType . peekFieldRaw (peekList peekTypeSpec) "sum"
  , fmap RecType . peekFieldRaw (peekMap peekName peekTypeSpec) "record"
  , \i -> do
      dom <- peekFieldRaw (peekList peekTypeSpec) "domain" i
      cod <- peekFieldRaw (peekList peekTypeSpec) "codomain" i
      pure $ FunType dom cod
  , const (pure AnyType)
  ] idx
