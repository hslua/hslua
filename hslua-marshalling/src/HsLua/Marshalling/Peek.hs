{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : HsLua.Marshalling.Peek
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : Portable

Functions which unmarshal and retrieve Haskell values from Lua's stack.
-}
module HsLua.Marshalling.Peek
  ( Peeker
  , Result (..)
  , isFailure
  , failure
  , force
  , retrieving
  , resultToEither
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
  , peekName
  -- * Readable types
  , peekRead
  -- * Collections
  , peekKeyValuePairs
  , peekList
  , peekMap
  , peekSet
  -- * Combinators
  , optional
  , peekFieldRaw
  , peekPair
  -- * Helpers
  , reportValueOnFailure
  , typeMismatchMessage
  -- * Lua peek monad
  , LuaPeek (..)
  , runLuaPeek
  , withContext
  ) where

import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String (IsString (fromString))
import HsLua.Core as Lua
import Text.Read (readMaybe)

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as Utf8

-- | Record to keep track of failure contexts while retrieving objects
-- from the Lua stack.
data Result a
  = Success a
  | Failure ByteString [Name]
  deriving (Show, Eq, Functor)

instance Applicative Result where
  pure = Success
  Success f         <*> s = fmap f s
  Failure msg stack <*> _         = Failure msg stack

instance Monad Result where
  Failure msg stack >>= _ = Failure msg stack
  Success x         >>= f = f x

-- | Lua operation with an additional failure mode that can stack errors
-- from different contexts; errors are not based on exceptions).
newtype LuaPeek e a = LuaPeek (LuaE e (Result a))
  deriving (Functor)

-- | The inverse of LuaPeek.
runLuaPeek :: LuaPeek e a -> LuaE e (Result a)
runLuaPeek (LuaPeek x) = x

instance Applicative (LuaPeek e) where
  pure = LuaPeek . return . pure
  {-# INLINE pure #-}

  LuaPeek f <*> x = LuaPeek $ f >>= \case
    Failure msg stack -> return $ Failure msg stack
    Success f'        -> fmap f' <$> runLuaPeek x
  {-# INLINEABLE (<*>) #-}

  m *> k = m >>= const k
  {-# INLINE (*>) #-}

instance Monad (LuaPeek e) where
  LuaPeek m >>= k = LuaPeek $
    m >>= \case
      Failure msg stack -> return $ Failure msg stack
      Success x         -> runLuaPeek (k x)

-- | Transform the result using the given function.
withContext :: Name -> LuaPeek e a -> LuaPeek e a
withContext ctx = LuaPeek . retrieving ctx . runLuaPeek


-- | Returns 'True' iff the peek result is a Failure.
isFailure :: Result a -> Bool
isFailure Failure {} = True
isFailure _          = False

-- | Combines the peek failure components into a reportable string.
formatPeekFailure :: ByteString -> [Name] -> String
formatPeekFailure msg stack =
  intercalate "\n\twhile retrieving " $
  map Utf8.toString (msg : map fromName (reverse stack))

-- | Function to retrieve a value from Lua's stack.
type Peeker e a = StackIndex -> LuaE e (Result a)

-- | Create a peek failure record from an error message.
failure :: ByteString -> Result a
failure msg = Failure msg []

-- | Add a message to the peek traceback stack.
addFailureContext :: Name -> Result a -> Result a
addFailureContext name = \case
  Failure msg stack -> Failure msg (name : stack)
  x -> x

-- | Add context information to the peek traceback stack.
retrieving :: Name
           -> LuaE e (Result a)
           -> LuaE e (Result a)
retrieving msg = fmap (addFailureContext msg)

-- | Force creation of an unwrapped result, throwing an exception if
-- that's not possible.
force :: LuaError e => Result a -> LuaE e a
force = \case
  Success x -> return x
  Failure msg stack -> failLua $ formatPeekFailure msg stack

-- | Converts a Result into an Either, where @Left@ holds the reportable
-- string in case of an failure.
resultToEither :: Result a -> Either String a
resultToEither = \case
  Failure msg stack -> Left $ formatPeekFailure msg stack
  Success x         -> Right x

-- | Converts an old peek funtion to a 'Peeker'.
toPeeker :: LuaError e
         => (StackIndex -> LuaE e a)
         -> Peeker e a
toPeeker op idx = try (op idx) >>= \case
  Left err  -> return $ failure $ Utf8.fromString (show err)
  Right res -> return $ Success res

-- | Use @test@ to check whether the value at stack index @n@ has
-- the correct type and use @peekfn@ to convert it to a Haskell
-- value if possible. A successfully received value is wrapped
-- using the 'Right' constructor, while a type mismatch results
-- in @Left PeekError@ with the given error message.
typeChecked :: LuaError e
            => Name                         -- ^ expected type
            -> (StackIndex -> LuaE e Bool)  -- ^ pre-condition checker
            -> Peeker e a
            -> Peeker e a
typeChecked expectedType test peekfn idx = do
  v <- test idx
  if v
    then peekfn idx
    else failure <$> typeMismatchMessage expectedType idx

-- | Generate a type mismatch error.
typeMismatchMessage :: LuaError e
                    => Name       -- ^ expected type
                    -> StackIndex -- ^ index of offending value
                    -> LuaE e ByteString
typeMismatchMessage (Name expected) idx = do
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
reportValueOnFailure :: LuaError e
                     => Name         -- ^ expected type
                     -> (StackIndex -> LuaE e (Maybe a))
                     -> Peeker e a
reportValueOnFailure expected peekMb idx = do
  res <- peekMb idx
  case res of
    Just x  -> return $ Success x
    Nothing -> failure <$> typeMismatchMessage expected idx

-- | Retrieves a 'Bool' as a Lua boolean.
peekBool :: Peeker e Bool
peekBool = fmap Success . toboolean

--
-- Strings
--

-- | Like 'tostring', but ensures that the value at the given index is
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
peekLazyByteString = fmap (fmap BL.fromStrict) . peekByteString

-- | Retrieves a 'String' from an UTF-8 encoded Lua string.
peekString :: LuaError e => Peeker e String
peekString = peekStringy

-- | Retrieves a String-like value from an UTF-8 encoded Lua string.
--
-- This should not be used to peek 'ByteString' values or other values
-- for which construction via 'fromString' can result in loss of
-- information.
peekStringy :: forall a e. (LuaError e, IsString a) => Peeker e a
peekStringy = fmap (fmap $ fromString . Utf8.toString) . peekByteString

-- | Retrieves a 'T.Text' value as an UTF-8 encoded string.
peekText :: LuaError e => Peeker e T.Text
peekText = fmap (fmap Utf8.toText) . peekByteString

-- | Retrieves a Lua string as 'Name'.
peekName :: LuaError e => Peeker e Name
peekName = fmap (fmap Name) . peekByteString

--
-- Arbitrary values
--

-- | Retrieves a value by getting a String from Lua, then using
-- 'readMaybe' to convert the String into a Haskell value.
peekRead :: forall a e. (LuaError e, Read a) => Peeker e a
peekRead = fmap (>>= readValue) . peekString
  where
    readValue s = case readMaybe s of
      Just x  -> Success x
      Nothing -> failure $ "Could not read: " <> Utf8.fromString s

--
-- Numbers
--

-- | Retrieves an 'Integral' value from the Lua stack.
peekIntegral :: forall a e. (LuaError e, Integral a, Read a) => Peeker e a
peekIntegral idx =
  ltype idx >>= \case
    TypeNumber  -> fmap fromIntegral <$>
                   reportValueOnFailure "Integral" tointeger idx
    TypeString  -> do
      str <- fromMaybe (Prelude.error "programming error in peekIntegral")
             <$> tostring idx
      msg <- typeMismatchMessage "Integral" idx
      return $ maybe (failure msg) Success $ readMaybe (Utf8.toString str)
    _ -> failure <$> typeMismatchMessage "Integral" idx

-- | Retrieve a 'RealFloat' (e.g., 'Float' or 'Double') from the stack.
peekRealFloat :: forall a e. (LuaError e, RealFloat a, Read a) => Peeker e a
peekRealFloat idx =
  ltype idx >>= \case
    TypeString  -> do
      str <- fromMaybe (Prelude.error "programming error in peekRealFloat")
             <$> tostring idx
      msg <- typeMismatchMessage "RealFloat" idx
      return $ maybe (failure msg) Success $ readMaybe (Utf8.toString str)
    _ -> fmap realToFrac <$> reportValueOnFailure "RealFloat" tonumber idx

-- | Reads a numerically indexed table @t@ into a list, where the 'length' of
-- the list is equal to @rawlen(t)@. The operation will fail unless all
-- numerical fields between @1@ and @rawlen(t)@ can be retrieved.
peekList :: forall a e. LuaError e => Peeker e a -> Peeker e [a]
peekList peekElement = fmap (retrieving "list") .
  typeChecked "table" istable $ \idx -> do
  let elementsAt [] = return []
      elementsAt (i : is) = do
        x  <- LuaPeek . retrieving ("index " <> showInt i) $
              rawgeti idx i *> peekElement top <* pop 1
        xs <- elementsAt is
        return (x:xs)
      showInt (Lua.Integer x) = fromString $ show x
  listLength <- fromIntegral <$> rawlen idx
  runLuaPeek $ elementsAt [1..listLength]

-- | Retrieves a key-value Lua table as 'Map'.
peekMap :: (LuaError e, Ord a)
        => Peeker e a -> Peeker e b -> Peeker e (Map a b)
peekMap keyPeeker valuePeeker = retrieving "Map"
  . fmap (fmap Map.fromList)
  . peekKeyValuePairs keyPeeker valuePeeker

-- | Read a table into a list of pairs.
peekKeyValuePairs :: LuaError e
                  => Peeker e a -> Peeker e b -> Peeker e [(a, b)]
peekKeyValuePairs keyPeeker valuePeeker =
  typeChecked "table" istable $ \idx -> do
    idx' <- absindex idx
    let remainingPairs = do
          LuaPeek (nextPair keyPeeker valuePeeker idx') >>= \case
            Nothing -> return []
            Just a  -> (a:) <$> remainingPairs
    pushnil
    runLuaPeek remainingPairs

-- | Get the next key-value pair from a table. Assumes the last
-- key to be on the top of the stack and the table at the given
-- index @idx@. The next key, if it exists, is left at the top of
-- the stack.
nextPair :: LuaError e
         => Peeker e a -> Peeker e b -> Peeker e (Maybe (a, b))
nextPair keyPeeker valuePeeker idx = retrieving "key-value pair" $ do
  hasNext <- next idx
  if not hasNext
    then return $ Success Nothing
    else do
      key   <- retrieving "key"   $ keyPeeker   (nth 2)
      value <- retrieving "value" $ valuePeeker (nth 1)
      pop 1    -- remove value, leave the key
      return $ curry Just <$> key <*> value

-- | Retrieves a 'Set' from an idiomatic Lua representation. A
-- set in Lua is idiomatically represented as a table with the
-- elements as keys. Elements with falsy values are omitted.
peekSet :: (LuaError e, Ord a) => Peeker e a -> Peeker e (Set a)
peekSet elementPeeker = retrieving "Set"
  . fmap (fmap (Set.fromList . map fst . filter snd))
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
    then return $ Success Nothing
    else fmap Just <$> peeker idx

-- | Get value at key from a table.
peekFieldRaw :: LuaError e => Peeker e a -> Name -> Peeker e a
peekFieldRaw peeker name = typeChecked "table" Lua.istable $ \idx ->
  retrieving ("raw field '" <> name <> "'") $ do
    absidx <- Lua.absindex idx
    pushstring $ fromName name
    rawget absidx
    peeker top <* Lua.pop 1

-- | Retrieves a value pair from a table. Expects the values to be
-- stored in a numerically indexed table; does not access metamethods.
peekPair :: LuaError e
         => (Peeker e a, Peeker e b)
         -> Peeker e (a, b)
peekPair (peekA, peekB) idx = do
  idx' <- absindex idx
  a <- rawgeti idx' 1 *> peekA top
  b <- rawgeti idx' 2 *> peekB top
  Lua.pop 2
  return $ (,) <$> a <*> b
