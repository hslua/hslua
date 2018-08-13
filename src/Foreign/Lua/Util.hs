{-|
Module      : Foreign.Lua.Util
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : portable

HsLua utility functions.
-}
module Foreign.Lua.Util
  ( getglobal'
  , setglobal'
  , run
  , runEither
  , raiseError
  , Optional (Optional, fromOptional)
  -- * getting values
  , peekEither
  , popValue
  ) where

import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Foreign.Lua.Core (Lua, NumResults, StackIndex)
import Foreign.Lua.Types (Peekable, Pushable)

import qualified Data.ByteString as B
import qualified Foreign.Lua.Core as Lua
import qualified Foreign.Lua.Types as Lua

-- | Run lua computation using the default HsLua state as starting point. Raised
-- exceptions are passed through; error handling is the responsibility of the
-- caller.
run :: Lua a -> IO a
run = (Lua.newstate `bracket` Lua.close) . flip Lua.runWith

-- | Run the given Lua computation; exceptions raised in haskell code are
-- caught, but other exceptions (user exceptions raised in haskell, unchecked
-- type errors, etc.) are passed through.
runEither :: Lua a -> IO (Either Lua.Exception a)
runEither = try . run

-- | Like @getglobal@, but knows about packages and nested tables. E.g.
--
-- > getglobal' "math.sin"
--
-- will return the function @sin@ in package @math@.
getglobal' :: ByteString -> Lua ()
getglobal' = getnested . splitdot

-- | Like @setglobal@, but knows about packages and nested tables. E.g.
--
-- > pushstring "0.9.4"
-- > setglobal' "mypackage.version"
--
-- All tables and fields, except for the last field, must exist.
setglobal' :: ByteString -> Lua ()
setglobal' s =
  case reverse (splitdot s) of
    [] ->
      return ()
    [_] ->
      Lua.setglobal s
    (lastField : xs) -> do
      getnested (reverse xs)
      Lua.pushvalue (Lua.nthFromTop 2)
      Lua.setfield (Lua.nthFromTop 2) lastField
      Lua.pop 1

-- | Gives the list of the longest substrings not containing dots.
splitdot :: ByteString -> [ByteString]
splitdot = filter (/= B.singleton dot) .
           B.groupBy (\a b -> a /= dot && b /= dot)
  where dot = fromIntegral (ord '.')

-- | Pushes the value described by the strings to the stack; where the first
-- value is the name of a global variable and the following strings are the
-- field values in nested tables.
getnested :: [ByteString] -> Lua ()
getnested [] = return ()
getnested (x:xs) = do
  Lua.getglobal x
  mapM_ (\a -> Lua.getfield Lua.stackTop a *> Lua.remove (Lua.nthFromTop 2)) xs

-- | Raise a Lua error, using the given value as the error object.
raiseError :: Pushable a => a -> Lua NumResults
raiseError e = do
  Lua.push e
  Lua.error
{-# INLINABLE raiseError #-}

-- | Newtype wrapper intended to be used for optional Lua values. Nesting this
-- type is strongly discouraged as missing values on inner levels are
-- indistinguishable from missing values on an outer level; wrong values
-- would be the likely result.
newtype Optional a = Optional { fromOptional :: Maybe a }

instance Peekable a => Peekable (Optional a) where
  safePeek idx = do
    noValue <- Lua.isnoneornil idx
    if noValue
      then return . return $ Optional Nothing
      else fmap (Optional . Just) <$> Lua.safePeek idx

instance Pushable a => Pushable (Optional a) where
  push (Optional Nothing)  = Lua.pushnil
  push (Optional (Just x)) = Lua.push x


--
-- Getting Values
--

-- | Try to convert the value at the given stack index to a Haskell value.
-- Returns @Left@ with an error message on failure.
peekEither :: Peekable a => StackIndex -> Lua (Either ByteString a)
peekEither idx = Lua.safePeek idx >>= return . \case
  Lua.Success x -> Right x
  Lua.Error msgs -> Left (mconcat msgs)

-- | Get, then pop the value at the top of the stack. The pop operation is
-- executed even if the retrieval operation failed.
popValue :: Peekable a => Lua a
popValue = do
  resOrError <- Lua.safePeek Lua.stackTop
  Lua.pop 1
  Lua.force resOrError
{-# INLINABLE popValue #-}
