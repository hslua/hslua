{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Peek
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which unmarshal and retrieve Haskell values from Lua's stack.
-}
module HsLua.Peek
  ( Peeker
  , PeekError (..)
  , errorMsg
  , force
  , formatPeekError
  , pushMsg
  , toPeeker
  -- * Primitives
  , peekBool
  , peekIntegral
  , peekRealFloat
  -- * Strings
  , peekByteString
  , peekLazyByteString
  , peekString
  , peekText
  , peekStringy
  -- * Collections
  , peekKeyValuePairs
  , peekList
  , peekMap
  , peekSet
  -- * Combinators
  , optional
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString (fromString))
import Data.Text (Text)
import HsLua.Core as Lua
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as Utf8

-- | List of errors which occurred while retrieving a value from
-- the stack.
newtype PeekError = PeekError { fromPeekError :: NonEmpty Text }
  deriving (Eq, Show)

formatPeekError :: PeekError -> String
formatPeekError (PeekError msgs) = T.unpack $
  T.intercalate "\n\t" (NonEmpty.toList msgs)

-- | Function to retrieve a value from Lua's stack.
type Peeker e a = StackIndex -> LuaE e (Either PeekError a)

-- | Create a peek error from an error message.
errorMsg :: ByteString -> PeekError
errorMsg  = PeekError . pure . Utf8.toText

-- | Add a message to the peek traceback stack.
pushMsg :: Text -> PeekError -> PeekError
pushMsg msg (PeekError lst) = PeekError $ msg <| lst

-- | Add context information to the peek traceback stack.
retrieving :: Text -> Either PeekError a -> Either PeekError a
retrieving msg = first $ pushMsg ("retrieving " <> msg)

-- | Force creation of a result, throwing an exception if that's
-- not possible.
force :: LuaError e => Either PeekError a -> LuaE e a
force = either (failLua . formatPeekError) return

-- | Convert an old peek funtion to a 'Peeker'.
toPeeker :: LuaError e
         => (StackIndex -> LuaE e a)
         -> Peeker e a
toPeeker op idx =
  (Right <$> op idx) <|> return (Left $ errorMsg "retrieving failed")

-- | Use @test@ to check whether the value at stack index @n@ has
-- the correct type and use @peekfn@ to convert it to a Haskell
-- value if possible. A successfully received value is wrapped
-- using the 'Right' constructor, while a type mismatch results
-- in @Left PeekError@ with the given error message.
typeChecked :: LuaError e
            => ByteString                   -- ^ expected type
            -> (StackIndex -> LuaE e Bool)  -- ^ pre-condition checker
            -> Peeker e a
            -> Peeker e a
typeChecked expectedType test peekfn idx = do
  v <- test idx
  if v
    then peekfn idx
    else Left <$> typeMismatchError expectedType idx

-- | Generate a type mismatch error.
typeMismatchError :: LuaError e
                  => ByteString -- ^ expected type
                  -> StackIndex -- ^ index of offending value
                  -> LuaE e PeekError
typeMismatchError expected idx = do
  pushTypeMismatchError expected idx
  tostring top >>= \case
    Just !msg -> errorMsg msg <$ pop 1
    Nothing  -> failLua $ mconcat
      [ "Unknown type mismatch for "
      , Utf8.toString expected
      , " at stack index "
      , show (fromStackIndex idx)
      ]

-- | Report the expected and actual type of the value under the given
-- index if conversion failed.
reportValueOnFailure :: LuaError e
                     => Text
                     -> (StackIndex -> LuaE e (Maybe a))
                     -> Peeker e a
reportValueOnFailure expected peekMb idx = do
  res <- peekMb idx
  case res of
    Just x  -> return $ Right x
    Nothing -> Left <$> typeMismatchError (Utf8.fromText expected) idx

-- | Retrieves a 'Bool' as a Lua boolean.
peekBool :: Peeker e Bool
peekBool = fmap Right . toboolean

--
-- Strings
--

-- | Like @'tostring', but ensures that the value at the given index is
-- not silently converted to a string, as would happen with numbers.
toByteString :: StackIndex -> LuaE e (Maybe ByteString)
toByteString idx = do
  -- copy value, as tostring converts numbers to strings *in-place*.
  pushvalue idx
  tostring top <* pop 1

-- | Retrieves a 'ByteString' as a raw string.
peekByteString :: LuaError e => Peeker e ByteString
peekByteString = reportValueOnFailure "string" toByteString

-- | Retrieves a lazy 'BL.ByteString' as a raw string.
peekLazyByteString :: LuaError e => Peeker e BL.ByteString
peekLazyByteString = fmap (second BL.fromStrict) . peekByteString

-- | Retrieves a 'String' from an UTF-8 encoded Lua string.
peekString :: LuaError e => Peeker e String
peekString = peekStringy

-- | Retrieves a String-like value from an UTF-8 encoded Lua string.
--
-- This should not be used to peek 'ByteString' values or other values
-- for which construction via 'fromString' can result in loss of
-- information.
peekStringy :: forall a e. (LuaError e, IsString a) => Peeker e a
peekStringy = fmap (second $ fromString . Utf8.toString) . peekByteString

-- | Retrieves a 'T.Text' value as an UTF-8 encoded string.
peekText :: LuaError e => Peeker e T.Text
peekText = fmap (second Utf8.toText) . peekByteString

--
-- Numbers
--

-- | Retrieves an 'Integral' value from the Lua stack.
peekIntegral :: forall a e. (LuaError e, Integral a, Read a) => Peeker e a
peekIntegral idx =
  ltype idx >>= \case
    TypeNumber  -> second fromIntegral <$>
                   reportValueOnFailure "Integral" tointeger idx
    TypeString  -> do
      str <- fromMaybe (Prelude.error "programming error in peekIntegral")
             <$> tostring idx
      let msg = "expected Integral, got '" <> str <> "' (string)"
      return $ maybe (Left $ errorMsg msg) Right $ readMaybe (Utf8.toString str)
    _ -> Left <$> typeMismatchError "Integral" idx

-- | Retrieve a 'RealFloat' (e.g., 'Float' or 'Double') from the stack.
peekRealFloat :: forall a e. (LuaError e, RealFloat a, Read a) => Peeker e a
peekRealFloat idx =
  ltype idx >>= \case
    TypeString  -> do
      str <- fromMaybe (Prelude.error "programming error in peekRealFloat")
             <$> tostring idx
      let msg = "expected RealFloat, got '" <> str <> "' (string)"
      return $ maybe (Left $ errorMsg msg) Right $ readMaybe (Utf8.toString str)
    _ -> second realToFrac <$>
         reportValueOnFailure "RealFloat" tonumber idx

-- | Reads a numerically indexed table @t@ into a list, where the 'length' of
-- the list is equal to @#t@. The operation will fail if a numerical field @n@
-- with @1 ≤ n < #t@ is missing.
peekList :: forall a e. LuaError e => Peeker e a -> Peeker e [a]
peekList peekElement = typeChecked "table" istable $ \idx -> do
  let elementsAt [] = return (Right [])
      elementsAt (i : is) = do
        eitherX <- rawgeti idx i *> peekElement top <* pop 1
        case eitherX of
          Right x  -> second (x:) <$> elementsAt is
          Left err -> return . Left $
                      pushMsg ("in field " <> T.pack (show i)) err
  listLength <- fromIntegral <$> rawlen idx
  elementsAt [1..listLength]

-- | Retrieves a key-value Lua table as 'Map'.
peekMap :: (LuaError e, Ord a)
        => Peeker e a -> Peeker e b -> Peeker e (Map a b)
peekMap keyPeeker valuePeeker =
    fmap (retrieving "Map" . second Map.fromList)
  . peekKeyValuePairs keyPeeker valuePeeker

-- | Read a table into a list of pairs.
peekKeyValuePairs :: LuaError e
                  => Peeker e a -> Peeker e b -> Peeker e [(a, b)]
peekKeyValuePairs keyPeeker valuePeeker =
  typeChecked "table" istable $ \idx -> do
    idx' <- absindex idx
    let remainingPairs = do
          res <- nextPair keyPeeker valuePeeker idx'
          case res of
            Left err       -> return $ Left err
            Right Nothing  -> return $ Right []
            Right (Just a) -> second (a:) <$> remainingPairs
    pushnil
    remainingPairs

-- | Get the next key-value pair from a table. Assumes the last
-- key to be on the top of the stack and the table at the given
-- index @idx@. The next key, if it exists, is left at the top of
-- the stack.
nextPair :: LuaError e
         => Peeker e a -> Peeker e b -> Peeker e (Maybe (a, b))
nextPair keyPeeker valuePeeker idx = retrieving "key-value pair" <$> do
  hasNext <- next idx
  if not hasNext
    then return $ Right Nothing
    else do
      key   <- retrieving "key"   <$> keyPeeker   (nth 2)
      value <- retrieving "value" <$> valuePeeker (nth 1)
      pop 1    -- remove value, leave the key
      return $ curry Just <$> key <*> value

-- | Retrieves a 'Set' from an idiomatic Lua representation. A
-- set in Lua is idiomatically represented as a table with the
-- elements as keys. Elements with falsy values are omitted.
peekSet :: (LuaError e, Ord a) => Peeker e a -> Peeker e (Set a)
peekSet elementPeeker =
    fmap (retrieving "Set" .
          second (Set.fromList . map fst . filter snd))
  . peekKeyValuePairs elementPeeker peekBool

--
-- Combinators
--

-- | Makes a result optional. Returns 'Nothing' if the Lua value
-- is @nil@; otherwise applies the peeker and returns its result.
optional :: Peeker e a -- ^ peeker
         -> Peeker e (Maybe a)
optional peeker idx = do
  noValue <- Lua.isnoneornil idx
  if noValue
    then return $ Right Nothing
    else fmap Just <$> peeker idx
