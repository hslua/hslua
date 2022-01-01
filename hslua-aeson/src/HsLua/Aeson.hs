{-|
Module      :  HsLua.Aeson
Copyright   :  © 2017–2021 Albert Krewinkel
License     :  MIT
Maintainer  :  Albert Krewinkel <tarleb@zeitkraut.de>

Pushes and retrieves aeson `Value`s to and from the Lua stack.

- `Null` values are encoded as a special value (stored in the
  registry field `HSLUA_AESON_NULL`).

- Objects are converted to string-indexed tables.

- Arrays are converted to sequence tables. Array-length is
  included as the value at index 0. This makes it possible to
  distinguish between empty arrays and empty objects.

- JSON numbers are converted to Lua numbers, i.e., 'Lua.Number';
  the exact C type may vary, depending on compile-time Lua
  configuration.
-}
module HsLua.Aeson
  ( peekValue
  , pushValue
  , peekViaJSON
  , pushViaJSON
  ) where

import Control.Monad ((<$!>))
import Data.Scientific (toRealFloat, fromFloatDigits)
import Data.Vector (Vector)
import Foreign.Ptr (nullPtr)
import HsLua.Core as Lua
import HsLua.Marshalling as Lua

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified HsLua.Core.Utf8 as UTF8

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (toText, fromText)
import qualified Data.Aeson.KeyMap as KeyMap
#else
import Data.Text (Text)
import qualified Data.HashMap.Strict as KeyMap

toText, fromText :: Text -> Text
toText = id
fromText = id
#endif

-- | Hslua StackValue instance for the Aeson Value data type.
pushValue :: LuaError e => Pusher e Aeson.Value
pushValue val = do
  checkstack' 1 "HsLua.Aeson.pushValue"
  case val of
    Aeson.Object o -> pushKeyValuePairs pushKey pushValue $ KeyMap.toList o
    Aeson.Number n -> pushRealFloat @Double $ toRealFloat n
    Aeson.String s -> pushText s
    Aeson.Array a  -> pushVector pushValue a
    Aeson.Bool b   -> pushBool b
    Aeson.Null     -> pushlightuserdata nullPtr
 where
  pushKey = pushText . toText

peekValue :: LuaError e => Peeker e Aeson.Value
peekValue idx = liftLua (ltype idx) >>= \case
  TypeBoolean -> Aeson.Bool  <$!> peekBool idx
  TypeNumber -> Aeson.Number . fromFloatDigits <$!> peekRealFloat @Double idx
  TypeString -> Aeson.String <$!> peekText idx
  TypeLightUserdata -> liftLua (touserdata idx) >>= \case
    -- must be the null pointer
    Nothing -> pure Aeson.Null
    _       -> typeMismatchMessage "null" idx >>= failPeek
  TypeTable -> do
      let peekKey = fmap fromText . peekText
          peekArray = Aeson.Array . Vector.fromList <$!>
            (retrieving "vector" $! peekList peekValue idx)
      isInt <- liftLua $ rawgeti idx 0 *> isinteger top <* pop 1
      if isInt
        then peekArray
        else do
          rawlen' <- liftLua $ rawlen idx
          if rawlen' > 0
            then peekArray
            else Aeson.Object . KeyMap.fromList <$!>
                 peekKeyValuePairs peekKey peekValue idx
  TypeNil -> return Aeson.Null
  luaType -> fail ("Unexpected type: " ++ show luaType)

-- | Push a vector onto the stack.
pushVector :: LuaError e
           => Pusher e a
           -> Pusher e (Vector a)
pushVector pushItem !v = do
  checkstack' 3 "HsLua.Aeson.pushVector"
  pushList pushItem $ Vector.toList v
  pushIntegral (Vector.length v)
  rawseti (nth 2) 0

-- | Retrieves a value from the Lua stack via JSON.
peekViaJSON :: (Aeson.FromJSON a, LuaError e) => Peeker e a
peekViaJSON idx = do
  value <- peekValue idx
  case Aeson.fromJSON value of
    Aeson.Success x -> pure x
    Aeson.Error msg -> failPeek $ "failed to decode: " <>
                       UTF8.fromString msg

-- | Pushes a value to the Lua stack as a JSON-like value.
pushViaJSON :: (Aeson.ToJSON a, LuaError e) => Pusher e a
pushViaJSON = pushValue . Aeson.toJSON
