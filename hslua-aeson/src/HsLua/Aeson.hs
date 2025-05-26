{-|
Module      :  HsLua.Aeson
Copyright   :  © 2017-2024 Albert Krewinkel
License     :  MIT
Maintainer  :  Albert Krewinkel <tarleb@hslua.org>

Pushes and retrieves aeson `Value`s to and from the Lua stack.

- JSON @null@ values are encoded as light userdata containing the
  @NULL@ pointer.

- Objects are converted to string-indexed tables.

- Arrays are converted to sequence tables and are given a
  metatable. This makes it possible to distinguish between empty
  arrays and empty objects. The metatable is stored in the
  registry under key @\'HsLua JSON array\'@' (see also
  'jsonarray').

- JSON numbers are converted to Lua numbers, i.e., 'Lua.Number';
  the exact C type may vary, depending on compile-time Lua
  configuration.
-}
module HsLua.Aeson
  ( peekValue
  , pushValue
  , peekViaJSON
  , pushViaJSON
  , jsonarray
    -- * Encoding arbitrary objects
  , peekToAeson
  , pushToAeson
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>), void)
import Data.Aeson.Key (toText, fromText)
import Data.Scientific (toRealFloat, fromFloatDigits)
import Foreign.Ptr (nullPtr)
import HsLua.Core as Lua
import HsLua.Marshalling as Lua

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as B
import qualified Data.Vector as Vector
import qualified HsLua.Core.Utf8 as UTF8

-- | Hslua StackValue instance for the Aeson Value data type.
pushValue :: LuaError e => Pusher e Aeson.Value
pushValue val = do
  checkstack' 1 "HsLua.Aeson.pushValue"
  case val of
    Aeson.Object o -> pushKeyValuePairs pushKey pushValue $ KeyMap.toList o
    Aeson.Number n -> pushRealFloat @Double $ toRealFloat n
    Aeson.String s -> pushText s
    Aeson.Array a  -> pushArray a
    Aeson.Bool b   -> pushBool b
    Aeson.Null     -> pushlightuserdata nullPtr
 where
  pushKey = pushText . toText
  pushArray x = do
    checkstack' 4 "HsLua.Aeson.pushVector"
    pushList pushValue $ Vector.toList x
    void $ newmetatable jsonarray
    setmetatable (nth 2)

-- | Name of the registry slot holding the metatable given to
-- array tables. The registry entry can be replaced with a
-- different table if needed.
jsonarray :: Name
jsonarray = "HsLua JSON array"

-- | Retrieves an Aeson 'Aeson.Value' from the Lua stack.
peekValue :: LuaError e => Peeker e Aeson.Value
peekValue idx = liftLua (ltype idx) >>= \case
  TypeBoolean -> Aeson.Bool  <$!> peekBool idx
  TypeNumber -> Aeson.Number . fromFloatDigits <$!> peekRealFloat @Double idx
  TypeString -> Aeson.String <$!> peekText idx
  TypeLightUserdata -> liftLua (touserdata idx) >>= \case
    -- must be the null pointer
    Nothing -> pure Aeson.Null
    _       -> typeMismatchMessage "null" idx >>= failPeek
  TypeNil -> return Aeson.Null
  TypeTable -> peekValueViaMetatable idx <|> do
      liftLua $ checkstack' 2 "HsLua.Aeson.peekValue"
      let peekKey = fmap fromText . peekText
          peekArray = Aeson.Array . Vector.fromList <$!>
            (retrieving "vector" $! peekList peekValue idx)
          isarray = getmetatable idx >>= \case
            False ->
              -- check for nonempty sequence
              (/= TypeNil) <$> rawgeti idx 1 <* pop 1
            True  -> getmetatable' jsonarray >>= \case
              TypeTable -> rawequal (nth 1) (nth 2) <* pop 2
              _         -> pure False
      liftLua isarray >>= \case
        True  -> peekArray
        False -> Aeson.Object . KeyMap.fromList <$!>
                 peekKeyValuePairs peekKey peekValue idx
  _ -> peekValueViaMetatable idx

--
-- Peek via __toaeson metamethod
--

-- | Retrieves a JSON value by using special metafields or metamethods.
peekValueViaMetatable :: LuaError e => Peeker e Aeson.Value
peekValueViaMetatable idx = peekValueViaToaeson idx <|> peekValueViaTojson idx

-- | Retrieves a JSON value by calling an object's @__toaeson@
-- metamethod.
peekValueViaToaeson :: Peeker e Aeson.Value
peekValueViaToaeson idx = do
  absidx <- liftLua (absindex idx)
  liftLua (getmetafield absidx "__toaeson") >>= \case
    TypeNil -> failPeek "Object does not have a `__toaeson` metavalue."
    _ -> do
      fn <- peekToAeson top `lastly` pop 1
      fn absidx

peekValueViaTojson :: LuaError e => Peeker e Aeson.Value
peekValueViaTojson idx = do
  absidx <- liftLua $ absindex idx
  liftLua (getmetafield absidx "__tojson") >>= \case
    TypeNil ->
      failPeek "Object does not have a `__tojson` metamethod."
    _ -> do
      -- Try to use the field value as function
      liftLua $ do
        pushvalue absidx
        call 1 1
      json <- peekLazyByteString top `lastly` pop 1
      maybe (failPeek "Could not decode string") pure $ Aeson.decode json

-- | Type for the function that gets an Aeson value from a Lua object.
type ToAeson e = Peeker e Aeson.Value

-- | Lua type name for 'ToAeson' values.
typeNameToAeson :: Name
typeNameToAeson = "HsLua.ToAeson"

-- | Pushes a function that converts the object at a given index into a
-- 'Aeson.Value'.
pushToAeson :: Pusher e (ToAeson e)
pushToAeson val = do
  newhsuserdatauv val 0
  _ <- newudmetatable typeNameToAeson
  setmetatable (nth 2)

-- | Gets the 'ToAeson' function from a Lua userdata object.
peekToAeson :: Peeker e (ToAeson e)
peekToAeson idx =
  liftLua (fromuserdata idx typeNameToAeson) >>= \case
    Nothing -> typeMismatchMessage typeNameToAeson idx >>= failPeek
    Just ta -> return ta

--
-- Retrieving any value via JSON
--

-- | Retrieves a value from the Lua stack via JSON.
peekViaJSON :: (Aeson.FromJSON a, LuaError e) => Peeker e a
peekViaJSON idx = do
  value <- peekValue idx
  case Aeson.fromJSON value of
    Aeson.Success x -> pure x
    Aeson.Error msg -> failPeek $ "failed to decode: " `B.append`
                       UTF8.fromString msg

-- | Pushes a value to the Lua stack as a JSON-like value.
pushViaJSON :: (Aeson.ToJSON a, LuaError e) => Pusher e a
pushViaJSON = pushValue . Aeson.toJSON
