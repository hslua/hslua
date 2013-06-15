{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind -fno-warn-unused-binds -fno-warn-missing-signatures #-}

-- |
-- Module      : Scripting.Lua.ConfigFile
-- Copyright   : (c) Benjamin Geer 2011, 2013
--
-- License     : BSD3-style
--
-- Maintainer  : benjamin.geer@gmail.com
-- Stability   : alpha
-- Portability : portable, ffi
--
-- Reads configuration files written in Lua.  See @http:\/\/www.lua.org\/@
-- for more details.
module Scripting.Lua.ConfigFile
       (
         Config,
         openConfig,
         closeConfig,
         getBool,
         getString,
         getInt,
         getDouble,
         getList,
         getNestedLists,
         getAssocList,
         getListOfAssocLists,
         getNestedAssocLists,
         ConfigFileException
       ) where

import qualified Scripting.Lua as Lua
import Control.Exception (Exception, throwIO)
import Control.Monad.Reader
import Data.Typeable (Typeable)

-- | Represents an open configuration file.
data Config = Config Lua.LuaState

-- | Thrown when an error occurs in reading a configuration file.
data ConfigFileException = ConfigFileException String
                         deriving (Show, Typeable)
instance Exception ConfigFileException

-- The ReaderT monad transformer stores the Lua environment so we
-- don't have to pass it around so much.
type LuaIO a = ReaderT Lua.LuaState IO a

-- | Opens a config file and returns an opaque reference to the file.
-- You must close this reference using @close@ when you're done reading
-- the file.
openConfig :: FilePath -> IO Config
openConfig path = do
  l <- Lua.newstate
  loadResult <- Lua.loadfile l path
  callResult <- Lua.call l 0 0
  if loadResult /= 0 || callResult /= 0 then
    do
      errMsg <- Lua.tostring l (-1)
      throwIO $ ConfigFileException $ "cannot run config file: " ++ errMsg
    else return (Config l)

-- | Closes a configuration file.
closeConfig :: Config -> IO ()
closeConfig (Config l) =
  -- putStrLn "closing Lua"
  Lua.close l

-- | Returns a boolean value from a configuration file.  Returns @False@
-- if the value is not defined in the file.  Example:
--
-- > someVal = true
getBool :: Config -> String -> IO Bool
getBool (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TBOOLEAN) -> return v
    (Nothing, Lua.TNIL) -> return False
    (_, _) -> throwIO $ ConfigFileException $
              "expected boolean value: " ++ name

-- | Returns a string value from a configuration file.  Returns the
-- empty string if the value is not defined in the file.  Example:
--
-- > someVal = "foo"
getString :: Config -> String -> IO String
getString (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TSTRING) -> return v
    (Nothing, Lua.TNIL) -> return ""
    (_, _) -> throwIO $ ConfigFileException $
              "expected string value: " ++ name

-- | Returns an integer value from a configuration file.  Example:
--
-- > someVal = 2
getInt :: Config -> String -> IO (Maybe Int)
getInt (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TNUMBER) -> return (Just v)
    (Nothing, Lua.TNIL) -> return Nothing
    (_, _) -> throwIO $ ConfigFileException $
              "expected numeric value: " ++ name

-- | Returns a double value from a configuration file.  Example:
--
-- > someVal = 3.1415926
getDouble :: Config -> String -> IO (Maybe Double)
getDouble (Config l) name = do
  (val, valType) <- getGlobalVal l name
  case (val, valType) of
    (Just v, Lua.TNUMBER) -> return (Just v)
    (Nothing, Lua.TNIL) -> return Nothing
    (_, _) -> throwIO $ ConfigFileException $
              "expected numeric value: " ++ name

-- | Returns a list of strings (i.e. a Lua table in which the keys
-- are integers and the values are strings) from a configuration file.
-- Example:
--
-- > someVal = { "foo", "bar", "baz" }
getList :: Config -> String -> IO [String]
getList (Config l) name =
  runReaderT (getTable name getListOfStrings) l

-- | Returns a list of lists, i.e. a Lua table of tables.  In the outer
-- table, the keys are integers and the values are tables, and in the inner
-- tables, the keys are integers and the values are strings.  Example:
--
-- > someVal = {
-- >    { "foo one", "foo two", "foo three" },
-- >    { "bar one", "bar two", "bar three" }
-- > }
getNestedLists :: Config -> String -> IO [[String]]
getNestedLists (Config l) name =
  runReaderT (getTable name (getOuterList getListOfStrings)) l

-- | Returns an association list, i.e. a Lua table in which the keys
-- and values are strings.  Example:
--
-- > someVal = {
-- >    one = "foo",
-- >    two = "bar",
-- >    three = "baz"
-- > }
getAssocList :: Config -> String -> IO [(String, String)]
getAssocList (Config l) name =
  runReaderT (getTable name getColumns) l

-- | Returns a list of association lists, i.e. a Lua table of tables.
-- In the outer table, the keys are integers and the values are tables,
-- and in the inner tables, the keys and values are strings.  Example:
--
-- > someVal = {
-- >    {
-- >       foo = "aaa",
-- >       bar = "bbb",
-- >       baz = "ccc"
-- >    },
-- >    {
-- >       foo = "ddd",
-- >       bar = "eee",
-- >       baz = "fff"
-- >    }
-- > }
getListOfAssocLists :: Config -> String -> IO [[(String, String)]]
getListOfAssocLists (Config l) name =
  runReaderT (getTable name (getOuterList getColumns)) l

-- | Returns an association list of association lists, i.e. a Lua table
-- of tables.  In the outer table, the keys are strings and the values
-- are tables, and in the inner tables, the keys and values are strings.
-- Example:
--
-- > someVal = {
-- >    something = {
-- >       foo = "aaa",
-- >       bar = "bbb",
-- >       baz = "ccc"
-- >    },
-- >    somethingElse = {
-- >       foo = "ddd",
-- >       bar = "eee",
-- >       baz = "fff"
-- >    }
-- > }
getNestedAssocLists :: Config -> String -> IO [(String, [(String, String)])]
getNestedAssocLists (Config l) name =
  runReaderT (getTable name getRows) l

-- Private functions

{-

Gets a Lua global and pops it off the Lua stack.

-}
getGlobalVal l name = do
  Lua.getglobal l name
  val <- Lua.peek l (-1)
  valType <- Lua.ltype l (-1)
  Lua.pop l 1
  return (val, valType)

{-

Checks whether a value can be converted to a string.

-}
canBeString valType =
  valType `elem` [Lua.TSTRING, Lua.TNUMBER]

{-

Gets a Lua table, performs some action on it and returns the result
as a list.

-}
getTable :: String ->
            (String -> LuaIO [a]) ->
            LuaIO [a]
getTable name f = do
  l <- ask
  getglobal l name
  valType <- ltype l (-1)
  case valType of
    Lua.TTABLE -> do items <- f name
                     pop l 1
                     return items
    Lua.TNIL -> return []
    _ -> liftIO $ throwIO $ ConfigFileException $ "expected table: " ++ name


{-

Iterates over the elements of a Lua table whose keys are integers,
performs some action on each element, and returns the results as a
list.

-}
forList :: LuaIO a ->
           LuaIO [a]
forList f = do
  l <- ask
  tableSize <- objlen l (-1)
  forM [1..tableSize] $ \i -> do
    push l i
    gettable l (-2)
    f

{-

Gets all elements from a Lua table representing a list.  Keys are
integers and values are strings.

-}
getListOfStrings :: String ->
                    LuaIO [String]
getListOfStrings name = do
  l <- ask
  forList $ do
    valType <- ltype l (-1)
    if canBeString valType then
      do
        valStr <- tostring l (-1)
        pop l 1
        return valStr
      else liftIO $ throwIO $ ConfigFileException $
           "expected table of strings: " ++ name

{-

Gets all elements from a Lua table of tables.  In the outer table,
keys are integers and values are tables.  The function passed as an
argument knows the structure of the inner tables.

-}
getOuterList :: (String -> LuaIO a) ->
                String ->
                LuaIO [a]
getOuterList f name = do
  l <- ask
  forList $ do
    valType <- ltype l (-1)
    case valType of
      Lua.TTABLE -> do innerItems <- f name
                       pop l 1
                       return innerItems
      _ -> liftIO $ throwIO $ ConfigFileException $ "expected table: " ++ name

{-

Gets all elements from a Lua table of tables.  In the outer table,
each key is a string, and each value is a table.  In the inner tables,
keys and values are strings.

-}
getRows :: String -> LuaIO [(String, [(String, String)])]
getRows name = do
  l <- ask
  -- liftIO $ putStrLn $ "entering getRows"
  -- liftIO $ stackDump l
  pushnil l
  getRemainingRows name

{-

Recursively gets the remaining elements from a Lua table of tables.
In the outer table, each key is a string, and each value is a table.
In the inner tables, keys and values are strings.

-}
getRemainingRows :: String -> LuaIO [(String, [(String, String)])]
getRemainingRows name = do
  l <- ask
  -- liftIO $ putStrLn $ "entering getRemainingRows"
  -- liftIO $ stackDump l
  hasNext <- next l (-2)
  if hasNext then
    do
      -- liftIO $ putStrLn $ "getRemainingRows: hasNext"
      keyType <- ltype l (-2)
      valType <- ltype l (-1)
      case (keyType, valType) of
        (Lua.TSTRING, Lua.TTABLE) ->
          do keyStr <- tostring l (-2)
             columns <- getColumns name
             pop l 1
             rest <- getRemainingRows name
             return ((keyStr, columns) : rest)
        (_, _) -> liftIO $ throwIO $ ConfigFileException $
                  "expected string keys and table values: " ++ name
    else return []

{-

Gets all elements from a Lua table and returns them as a list of
key-value pairs, where keys and values are strings.

-}
getColumns :: String -> LuaIO [(String, String)]
getColumns name = do
  l <- ask
  -- liftIO $ putStrLn $ "entering getColumns"
  -- liftIO $ stackDump l
  pushnil l
  getRemainingColumns name

{-

Recursively gets the remaining elements from a Lua table and returns
them as a list of key-value pairs, where keys and values are strings.

-}
getRemainingColumns :: String -> LuaIO [(String, String)]
getRemainingColumns name = do
  l <- ask
  -- liftIO $ putStrLn $ "entering getRemainingColumns"
  -- liftIO $ stackDump l
  hasNext <- next l (-2)
  if hasNext then do
    -- liftIO $ putStrLn $ "getRemainingColumns: hasNext"
    -- liftIO $ stackDump l
    keyType <- ltype l (-2)
    valType <- ltype l (-1)
    if keyType == Lua.TSTRING && canBeString valType then
      do
        keyStr <- tostring l (-2)
        valStr <- tostring l (-1)
        pop l 1
        rest <- getRemainingColumns name
        return ((keyStr, valStr) : rest)
      else liftIO $ throwIO $ ConfigFileException $
           "expected string keys and string values: " ++ name
    else return []

{-

These are liftIO wrappers for the Lua functions we use, to reduce
clutter in the monadic code above.

-}
getglobal l name = liftIO $ Lua.getglobal l name
ltype l n = liftIO $ Lua.ltype l n
pop l n = liftIO $ Lua.pop l n
objlen l n = liftIO $ Lua.objlen l n
push l n = liftIO $ Lua.push l n
gettable l n = liftIO $ Lua.gettable l n
tostring l n = liftIO $ Lua.tostring l n
pushnil l = liftIO $ Lua.pushnil l
next l n = liftIO $ Lua.next l n

{-

Dumps the Lua stack for debugging purposes.

-}
stackDump l = do
  stackSize <- Lua.gettop l
  putStrLn "Stack dump:"
  forM_ (reverse [1..stackSize]) $ \i -> do
    let relativeIndex = stackSize - i + 1
    putStr $ "Index[" ++ show i ++ " / -" ++ show relativeIndex ++ "] = "
    itemType <- Lua.ltype l i
    case itemType of
      Lua.TNONE -> putStr "TNONE"
      Lua.TNIL -> putStr "TNIL"
      Lua.TBOOLEAN -> do boolVal <- Lua.toboolean l i
                         putStr $ "TBOOLEAN " ++ show boolVal
      Lua.TLIGHTUSERDATA -> putStr "TLIGHTUSERDATA"
      Lua.TNUMBER -> do iVal <- Lua.tointeger l i
                        putStr $ "TNUMBER " ++ show iVal
      Lua.TSTRING -> do sVal <- Lua.tostring l i
                        putStr $ "TSTRING " ++ sVal
      Lua.TTABLE -> putStr "TTABLE"
      Lua.TFUNCTION -> putStr "TFUNCTION"
      Lua.TUSERDATA -> putStr "TUSERDATA"
      Lua.TTHREAD -> putStr "TTHREAD"
    putStr "\n"
  putStr "\n"
