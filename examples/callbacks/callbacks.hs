{-# LANGUAGE OverloadedStrings #-}

-- An example hslua program that demonstrates providing Haskell callbacks
-- to Lua and getting Lua callbacks from Haskell.

import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Foreign.C.Types (CInt)
import Scripting.Lua as Lua
import Scripting.Lua.Raw as Lua

main :: IO ()
main = do
    callbacks <- newIORef []
    l <- newstate
    openlibs l
    registerrawhsfunction l "addLuaCallbacks" (addLuaCallbacks callbacks)
    registerrawhsfunction l "callLuaCallbacks" (callLuaCallbacks callbacks)
    registerrawhsfunction l "resetLuaCallbacks" (resetLuaCallbacks callbacks)
    loadfile l "examples/callbacks/callbacks.lua"
    call l 0 0
    close l

type LuaFunRef = Int

-- | Get Lua callbacks as argument to later call them in order.
-- Successive calls to this function without calling `resetLuaCallbacks`
-- adds more callbacks to the queue.
-- (I know lists are not the best functional queue implementations ...)
addLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
addLuaCallbacks cs l = do
    -- number of arguments passed to this function
    args <- gettop l
    -- make sure arguments are functions
    as <- checkArgs args
    case as of
      Nothing -> do
        -- arguments are functions, add them to callback queue and return
        -- nothing
        addCallbacks 1 args
        return 0
      Just errArg -> do
        -- error: argument at `errArg` is not a function, return error
        -- string
        pushstring l $ BC.pack $
          "argument " ++ show errArg ++ " is not a function"
        return 1
  where
    -- | Check if all arguments are functions, return `Just argIdx` if
    -- argument at `argIdx` is not a function and `Nothing` otherwise.
    checkArgs :: Int -> IO (Maybe Int)
    checkArgs 0 = return Nothing
    checkArgs n = do
      ty <- ltype l n
      if ty == TFUNCTION
        then checkArgs (n-1)
        else return $ Just n

    addCallbacks n max
      | n > max = return ()
      | otherwise = do
          -- move nth argument to top of the stack
          pushvalue l n
          -- add function reference to registry
          refId <- ref l registryindex
          -- add registry index to IORef
          modifyIORef cs (++ [refId])
          -- continue adding other arguments
          addCallbacks (n+1) max

-- | Call Lua callbacks collected with `addLuaCallbacks`.
callLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
callLuaCallbacks cs l = do
    cs' <- readIORef cs
    -- push new array to the stack
    createtable l (length cs') 0
    -- call callbacks and fill array with return values
    iter cs'
    return 1
  where
    iter [] = return ()
    iter (c : rest) = do
      getglobal2 l "table.insert"
      pushvalue l (-2)
      pushinteger l (fromIntegral c)
      gettable l registryindex
      -- call the callback
      pcall l 0 1 0
      -- call table.insert
      pcall l 2 0 0
      iter rest

-- | Reset callback queue and remove Lua functions from registry to enable
-- garbage collection.
resetLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
resetLuaCallbacks cs l = do
    cs' <- readIORef cs
    mapM_ (unref l registryindex) cs'
    writeIORef cs []
    return 0
