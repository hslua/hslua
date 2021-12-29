{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Marshalling.Peekers
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which unmarshal and retrieve Haskell values from Lua's stack.
-}
module HsLua.Marshalling.Peekers
  ( -- * Peeking values from the stack
    -- ** Primitives
    peekNil
  , peekNoneOrNil
  , peekBool
  , peekIntegral
  , peekRealFloat
    -- ** Strings
  , peekByteString
  , peekLazyByteString
  , peekString
  , peekText
  , peekStringy
  , peekName
  -- ** Readable types
  , peekRead
  -- ** Collections
  , peekKeyValuePairs
  , peekList
  , peekMap
  , peekSet
  -- ** Combinators
  , choice
  , peekFieldRaw
  , peekIndexRaw
  , peekPair
  , peekTriple
  -- ** Building peek functions
  , typeChecked
  , reportValueOnFailure
  , typeMismatchMessage
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad ((<$!>), (>=>), void)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString (fromString))
import HsLua.Core as Lua
import HsLua.Marshalling.Peek
import Text.Read (readMaybe)

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua.Core.Unsafe as Unsafe
import qualified HsLua.Core.Utf8 as Utf8

-- | Use @test@ to check whether the value at stack index @n@ has
-- the correct type and use @peekfn@ to convert it to a Haskell
-- value if possible. A successfully received value is wrapped
-- using the 'Right' constructor, while a type mismatch results
-- in @Left PeekError@ with the given error message.
typeChecked :: Name                         -- ^ expected type
            -> (StackIndex -> LuaE e Bool)  -- ^ pre-condition checker
            -> Peeker e a
            -> Peeker e a
typeChecked expectedType test peekfn idx = do
  v <- liftLua $ test idx
  if v
    then peekfn idx
    else typeMismatchMessage expectedType idx >>= failPeek

-- | Generate a type mismatch error.
typeMismatchMessage :: Name       -- ^ expected type
                    -> StackIndex -- ^ index of offending value
                    -> Peek e ByteString
typeMismatchMessage (Name expected) idx = liftLua $ do
  pushTypeMismatchError expected idx
  (tostring top <* pop 1) >>= \case
    Just !msg -> return msg
    Nothing  -> return $ mconcat
      [ "Unknown type mismatch for "
      , expected
      , " at stack index "
      , Utf8.fromString $ show (fromStackIndex idx)
      ]

-- | Report the expected and actual type of the value under the given
-- index if conversion failed.
reportValueOnFailure :: Name         -- ^ expected type
                     -> (StackIndex -> LuaE e (Maybe a))
                     -> Peeker e a
reportValueOnFailure expected peekMb idx = do
  res <- liftLua $ peekMb idx
  case res of
    Just x  -> return $! x
    Nothing -> typeMismatchMessage expected idx >>= failPeek

--
-- Primitives
--

-- | Succeeds if the value at the given index is @nil@.
peekNil :: Peeker e ()
peekNil = typeChecked "nil" Lua.isnil $ const (return ())
{-# INLINABLE peekNil #-}

-- | Succeeds if the given index is not valid or if the value at this
-- index is @nil@.
peekNoneOrNil :: Peeker e ()
peekNoneOrNil = typeChecked "none or nil" Lua.isnoneornil $ const (return ())
{-# INLINABLE peekNoneOrNil #-}

-- | Retrieves a 'Bool' as a Lua boolean.
peekBool :: Peeker e Bool
peekBool = liftLua . toboolean

--
-- Strings
--

-- | Like 'tostring', but ensures that the value at the given index is
-- not silently converted to a string, as would happen with numbers.
toByteString :: StackIndex -> LuaE e (Maybe ByteString)
toByteString idx = do
  -- Do an explicit type check, as @tostring@ converts numbers strings
  -- /in-place/, which we need to avoid.
  ltype idx >>= \case
    TypeString -> tostring idx
    _          -> checkstack 1 >>= \case
      False -> pure Nothing
      True  ->  do
        pushvalue idx
        tostring top <* pop 1
{-# INLINABLE toByteString #-}

-- | Retrieves a 'ByteString' as a raw string.
peekByteString :: Peeker e ByteString
peekByteString = reportValueOnFailure "string" toByteString
{-# INLINABLE peekByteString #-}

-- | Retrieves a lazy 'BL.ByteString' as a raw string.
peekLazyByteString :: Peeker e BL.ByteString
peekLazyByteString = (BL.fromStrict <$!>) . peekByteString
{-# INLINABLE peekLazyByteString #-}

-- | Retrieves a 'String' from an UTF-8 encoded Lua string.
peekString :: Peeker e String
peekString = peekStringy
{-# INLINABLE peekString #-}

-- | Retrieves a String-like value from an UTF-8 encoded Lua string.
--
-- This should not be used to peek 'ByteString' values or other values
-- for which construction via 'fromString' can result in loss of
-- information.
peekStringy :: forall a e. IsString a => Peeker e a
peekStringy = fmap (fromString . Utf8.toString) . peekByteString
{-# INLINABLE peekStringy #-}

-- | Retrieves a 'T.Text' value as an UTF-8 encoded string.
peekText :: Peeker e T.Text
peekText = (Utf8.toText <$!>) . peekByteString
{-# INLINABLE peekText #-}

-- | Retrieves a Lua string as 'Name'.
peekName :: Peeker e Name
peekName = (Name <$!>) . peekByteString
{-# INLINABLE peekName #-}

--
-- Arbitrary values
--

-- | Retrieves a value by getting a String from Lua, then using
-- 'readMaybe' to convert the String into a Haskell value.
peekRead :: forall a e. Read a => Peeker e a
peekRead = peekString >=> readValue
  where
    readValue s = case readMaybe s of
      Just x  -> pure x
      Nothing -> failPeek $ "Could not read: " <> Utf8.fromString s

--
-- Numbers
--

-- | Retrieves an 'Integral' value from the Lua stack.
peekIntegral :: forall a e. (Integral a, Read a) => Peeker e a
peekIntegral idx = liftLua (ltype idx) >>= \case
  TypeNumber  -> fromIntegral <$!>
                 reportValueOnFailure "Integral" tointeger idx
  TypeString  -> do
    Just str <- liftLua $ tostring idx
    case readMaybe (Utf8.toString str) of
      Nothing -> typeMismatchMessage "Integral" idx >>= failPeek
      Just x  -> return x
  _ -> typeMismatchMessage "Integral" idx >>= failPeek

-- | Retrieve a 'RealFloat' (e.g., 'Float' or 'Double') from the stack.
peekRealFloat :: forall a e. (RealFloat a, Read a) => Peeker e a
peekRealFloat idx = liftLua (ltype idx) >>= \case
  TypeString  -> do
    Just str <- liftLua $ tostring idx
    case readMaybe (Utf8.toString str) of
      Nothing -> typeMismatchMessage "RealFloat" idx >>= failPeek
      Just x  -> return x
  _ -> realToFrac <$!> reportValueOnFailure "RealFloat" tonumber idx

-- | Reads a numerically indexed table @t@ into a list, where the 'length' of
-- the list is equal to @rawlen(t)@. The operation will fail unless all
-- numerical fields between @1@ and @rawlen(t)@ can be retrieved.
peekList :: forall a e. LuaError e => Peeker e a -> Peeker e [a]
peekList peekElement = fmap (retrieving "list") .
  typeChecked "table" istable $ \idx -> do
  liftLua $ checkstack' 1 "retrieving a list"
  let elementsAt [] = return []
      elementsAt (i : is) = do
        x  <- retrieving ("index " <> showInt i) $
              liftLua (rawgeti idx i) *> peekElement top `lastly` pop 1
        xs <- elementsAt is
        return (x:xs)
      showInt (Lua.Integer x) = fromString $ show x
  listLength <- liftLua (rawlen idx)
  elementsAt [1..fromIntegral listLength]

-- | Retrieves a key-value Lua table as 'Map'.
peekMap :: (LuaError e, Ord a)
        => Peeker e a -> Peeker e b -> Peeker e (Map a b)
peekMap keyPeeker valuePeeker = retrieving "Map"
  . fmap Map.fromList
  . peekKeyValuePairs keyPeeker valuePeeker

-- | Read a table into a list of pairs.
peekKeyValuePairs :: LuaError e
                  => Peeker e a -> Peeker e b -> Peeker e [(a, b)]
peekKeyValuePairs keyPeeker valuePeeker =
  typeChecked "table" istable $ \idx -> cleanup $ do
    liftLua $ checkstack' 2 "retrieving key-value pairs"
    idx' <- liftLua $ absindex idx
    let remainingPairs = nextPair keyPeeker valuePeeker idx' >>= \case
          Nothing -> return []
          Just a  -> (a:) <$!> remainingPairs
    liftLua pushnil
    remainingPairs

-- | Get the next key-value pair from a table. Assumes the last
-- key to be on the top of the stack and the table at the given
-- index @idx@. The next key, if it exists, is left at the top of
-- the stack.
--
-- The key must be either nil or must exist in the table, or this
-- function will crash with an unrecoverable error.
nextPair :: Peeker e a -> Peeker e b -> Peeker e (Maybe (a, b))
nextPair keyPeeker valuePeeker idx = retrieving "key-value pair" $ do
  hasNext <- liftLua $ Unsafe.next idx
  if not hasNext
    then return Nothing
    else do
      key   <- retrieving "key"   $ keyPeeker   (nth 2)
      value <- retrieving "value" $ valuePeeker (nth 1)
      return (Just (key, value))
        `lastly` pop 1  -- remove value, leave the key

-- | Retrieves a 'Set' from an idiomatic Lua representation. A
-- set in Lua is idiomatically represented as a table with the
-- elements as keys. Elements with falsy values are omitted.
peekSet :: (LuaError e, Ord a) => Peeker e a -> Peeker e (Set a)
peekSet elementPeeker = withContext "Set"
  . fmap (Set.fromList . map fst . filter snd)
  . peekKeyValuePairs elementPeeker peekBool

--
-- Combinators
--

-- | Get value at key from a table.
peekFieldRaw :: LuaError e => Peeker e a -> Name -> Peeker e a
peekFieldRaw peeker name idx =
  retrieving ("raw field '" <> name <> "'") $! do
    liftLua $ do
      absidx <- Lua.absindex idx
      pushstring $ fromName name
      void (rawget absidx)
    peeker top `lastly` Lua.pop 1
{-# INLINABLE peekFieldRaw #-}

-- | Get value at integer index key from a table.
peekIndexRaw :: LuaError e => Lua.Integer -> Peeker e a -> Peeker e a
peekIndexRaw i peeker idx = do
  let showInt (Lua.Integer x) = fromString $ show x
  retrieving (fromString $ "raw index '" <> showInt i <> "'") $! do
    liftLua . void $ rawgeti idx i
    peeker top `lastly` Lua.pop 1
{-# INLINABLE peekIndexRaw #-}

-- | Retrieves a value pair from a table. Expects the values to be
-- stored in a numerically indexed table; does not access metamethods.
peekPair :: LuaError e
         => Peeker e a -> Peeker e b
         -> Peeker e (a, b)
peekPair peekA peekB idx = cleanup $ do
  liftLua $ checkstack' 2 "retrieving a pair"
  idx' <- liftLua $ absindex idx
  a <- liftLua (rawgeti idx' 1) *> peekA top
  b <- liftLua (rawgeti idx' 2) *> peekB top
  return (a, b)

-- | Retrieves a value triple from a table. Expects the values to be
-- stored in a numerically indexed table, with no metamethods.
peekTriple :: LuaError e
           => Peeker e a -> Peeker e b -> Peeker e c
           -> Peeker e (a, b, c)
peekTriple peekA peekB peekC idx = cleanup $ do
  liftLua $ checkstack' 3 "retrieving a triple"
  idx' <- liftLua $ absindex idx
  a <- liftLua (rawgeti idx' 1) *> peekA top
  b <- liftLua (rawgeti idx' 2) *> peekB top
  c <- liftLua (rawgeti idx' 3) *> peekC top
  return (a,b,c)

-- | Try all peekers and return the result of the first to succeed.
choice :: LuaError e
       => [Peeker e a]
       -> Peeker e a
choice peekers idx = case peekers of
  [] -> failPeek "all choices failed"
  p:ps -> p idx <|> choice ps idx
{-# INLINABLE choice #-}
