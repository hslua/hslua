{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Foreign.Lua.Peek
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which unmarshal and retrieve Haskell values from Lua's stack.
-}
module Foreign.Lua.Peek
  ( Peeker
  , PeekError (..)
  , errorMsg
  , force
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
  -- * Collections
  , peekKeyValuePairs
  , peekList
  , peekMap
  , peekSet
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Foreign.Lua.Core as Lua
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Foreign.Lua.Utf8 as Utf8

-- | List of errors which occurred while retrieving a value from
-- the stack.
newtype PeekError = PeekError { fromPeekError :: NonEmpty Text }
  deriving (Eq, Show)

formatPeekError :: PeekError -> String
formatPeekError (PeekError msgs) = T.unpack $
  T.intercalate "\n\t" (NonEmpty.toList msgs)

-- | Function to retrieve a value from Lua's stack.
type Peeker a = StackIndex -> Lua (Either PeekError a)

-- | Create a peek error from an error message.
errorMsg :: Text -> PeekError
errorMsg  = PeekError . pure

-- | Add a message to the peek traceback stack.
pushMsg :: Text -> PeekError -> PeekError
pushMsg msg (PeekError lst) = PeekError $ msg <| lst

-- | Add context information to the peek traceback stack.
retrieving :: Text -> Either PeekError a -> Either PeekError a
retrieving msg = first $ pushMsg ("retrieving " <> msg)

-- | Force creation of a result, throwing an exception if that's
-- not possible.
force :: Either PeekError a -> Lua a
force = either (throwMessage . formatPeekError) return

-- | Convert an old peek funtion to a 'Peeker'.
toPeeker :: (StackIndex -> Lua a)
         -> Peeker a
toPeeker op idx =
  (Right <$> op idx) <|> return (Left $ errorMsg "retrieving failed")

-- | Use @test@ to check whether the value at stack index @n@ has
-- the correct type and use @peekfn@ to convert it to a Haskell
-- value if possible. A successfully received value is wrapped
-- using the 'Right' constructor, while a type mismatch results
-- in @Left PeekError@ with the given error message.
typeChecked :: Text                      -- ^ expected type
            -> (StackIndex -> Lua Bool)  -- ^ pre-condition checker
            -> Peeker a
            -> Peeker a
typeChecked expectedType test peekfn idx = do
  v <- test idx
  if v
    then peekfn idx
    else Left <$> mismatchError expectedType idx

-- | Report the expected and actual type of the value under the given index if
-- conversion failed.
reportValueOnFailure :: Text
                     -> (StackIndex -> Lua (Maybe a))
                     -> Peeker a
reportValueOnFailure expected peekMb idx = do
  res <- peekMb idx
  case res of
    Just x  -> return $ Right x
    Nothing -> Left  <$> mismatchError expected idx

-- | Return a Result error containing a message about the assertion failure.
mismatchError :: Text -> StackIndex -> Lua PeekError
mismatchError expected idx = do
  actualType  <- ltype idx >>= typename
  actualValue <- Utf8.toText <$> tostring' idx <* pop 1
  return . errorMsg $
    "expected " <> expected <> ", got '" <>
    actualValue <> "' (" <> T.pack actualType <> ")"

-- | Retrieves a 'Bool' as a Lua boolean.
peekBool :: Peeker Bool
peekBool = fmap Right . toboolean

--
-- Strings
--

-- | Like @'tostring', but ensures that the value at the given index is
-- not silently converted to a string, as would happen with numbers.
toByteString :: StackIndex -> Lua (Maybe ByteString)
toByteString idx = do
  -- copy value, as tostring converts numbers to strings *in-place*.
  pushvalue idx
  tostring stackTop <* pop 1

-- | Retrieves a 'ByteString' as a raw string.
peekByteString :: Peeker ByteString
peekByteString = reportValueOnFailure "string" toByteString

-- | Retrieves a lazy 'BL.ByteString' as a raw string.
peekLazyByteString :: Peeker BL.ByteString
peekLazyByteString = fmap (second BL.fromStrict) . peekByteString

-- | Retrieves a 'String' as an UTF-8 encoded Lua string.
peekString :: Peeker String
peekString = fmap (second Utf8.toString) . peekByteString

-- | Retrieves a 'T.Text' value as an UTF-8 encoded string.
peekText :: Peeker T.Text
peekText = fmap (second Utf8.toText) . peekByteString

--
-- Numbers
--

-- | Retrieves an 'Integral' value from the Lua stack.
peekIntegral :: (Integral a, Read a) => Peeker a
peekIntegral idx =
  ltype idx >>= \case
    TypeNumber  -> second fromIntegral <$>
                   reportValueOnFailure "Integral" tointeger idx
    TypeString  -> do
      str <- Utf8.toString .
             fromMaybe (Prelude.error "programming error in peekIntegral")
             <$> tostring idx
      let msg = "expected Integral, got '" <> T.pack str <> "' (string)"
      return $ maybe (Left $ errorMsg msg) Right $ readMaybe str
    _ -> Left <$> mismatchError "Integral" idx

-- | Retrieve a 'RealFloat' (e.g., 'Float' or 'Double') from the stack.
peekRealFloat :: (RealFloat a, Read a) => Peeker a
peekRealFloat idx =
  ltype idx >>= \case
    TypeString  -> do
      str <- Utf8.toString .
             fromMaybe (Prelude.error "programming error in peekRealFloat")
             <$> tostring idx
      let msg = "expected RealFloat, got '" <> T.pack str <> "' (string)"
      return $ maybe (Left $ errorMsg msg) Right $ readMaybe str
    _ -> second realToFrac <$>
         reportValueOnFailure "RealFloat" tonumber idx

-- | Reads a numerically indexed table @t@ into a list, where the 'length' of
-- the list is equal to @#t@. The operation will fail if a numerical field @n@
-- with @1 ≤ n < #t@ is missing.
peekList :: Peeker a -> Peeker [a]
peekList peekElement = typeChecked "table" istable $ \idx -> do
  let elementsAt [] = return (Right [])
      elementsAt (i : is) = do
        eitherX <- rawgeti idx i *> peekElement (nthFromTop 1) <* pop 1
        case eitherX of
          Right x  -> second (x:) <$> elementsAt is
          Left err -> return . Left $
                      pushMsg ("in field " <> T.pack (show i)) err
  listLength <- fromIntegral <$> rawlen idx
  elementsAt [1..listLength]

-- | Retrieves a key-value Lua table as 'Map'.
peekMap :: Ord a => Peeker a -> Peeker b -> Peeker (Map a b)
peekMap keyPeeker valuePeeker =
    fmap (retrieving "Map" . second Map.fromList)
  . peekKeyValuePairs keyPeeker valuePeeker

-- | Read a table into a list of pairs.
peekKeyValuePairs :: Peeker a -> Peeker b -> Peeker [(a, b)]
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
nextPair :: Peeker a -> Peeker b -> Peeker (Maybe (a, b))
nextPair keyPeeker valuePeeker idx = retrieving "key-value pair" <$> do
  hasNext <- next idx
  if not hasNext
    then return $ Right Nothing
    else do
      key   <- retrieving "key"   <$> keyPeeker   (nthFromTop 2)
      value <- retrieving "value" <$> valuePeeker (nthFromTop 1)
      pop 1    -- remove value, leave the key
      return $ curry Just <$> key <*> value

-- | Retrieves a 'Set' from an idiomatic Lua representation. A
-- set in Lua is idiomatically represented as a table with the
-- elements as keys. Elements with falsy values are omitted.
peekSet :: Ord a => Peeker a -> Peeker (Set a)
peekSet elementPeeker =
    fmap (retrieving "Set" .
          second (Set.fromList . map fst . filter snd))
  . peekKeyValuePairs elementPeeker peekBool
