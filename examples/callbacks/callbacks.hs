{-# LANGUAGE OverloadedStrings #-}

-- An example hslua program that demonstrates providing Haskell callbacks
-- to Lua and getting Lua callbacks from Haskell.

import qualified Data.ByteString.Char8 as BC
import Control.Monad.Reader (ask, liftIO)
import Data.IORef
import Foreign.C.Types (CInt)
import Foreign.Lua as Lua

main :: IO ()
main = do
  callbacks <- newIORef []
  runLua $ do
    openlibs
    registerrawhsfunction "addLuaCallbacks" (addLuaCallbacks callbacks)
    registerrawhsfunction "callLuaCallbacks" (callLuaCallbacks callbacks)
    registerrawhsfunction "resetLuaCallbacks" (resetLuaCallbacks callbacks)
    loadfile "examples/callbacks/callbacks.lua"
    call 0 0

type LuaFunRef = Int

-- | Get Lua callbacks as argument to later call them in order.
-- Successive calls to this function without calling `resetLuaCallbacks`
-- adds more callbacks to the queue.
-- (I know lists are not the best functional queue implementations ...)
addLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
addLuaCallbacks cs l = runLuaWith l $ do
    -- number of arguments passed to this function
    args <- gettop
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
        pushstring $ BC.pack $
          "argument " ++ show errArg ++ " is not a function"
        return 1
  where
    -- | Check if all arguments are functions, return `Just argIdx` if
    -- argument at `argIdx` is not a function and `Nothing` otherwise.
    checkArgs :: StackIndex -> Lua (Maybe StackIndex)
    checkArgs 0 = return Nothing
    checkArgs n = do
      ty <- ltype n
      if ty == TFUNCTION
        then checkArgs (n-1)
        else return $ Just n

    addCallbacks :: StackIndex -> StackIndex -> Lua ()
    addCallbacks n max
      | n > max = return ()
      | otherwise = do
          -- move nth argument to top of the stack
          pushvalue n
          -- add function reference to registry
          refId <- ref registryindex
          -- add registry index to IORef
          liftIO $ modifyIORef cs (++ [refId])
          -- continue adding other arguments
          addCallbacks (n+1) max

-- | Call Lua callbacks collected with `addLuaCallbacks`.
callLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
callLuaCallbacks cs l = do
  cs' <- readIORef cs
  runLuaWith l $ do
    -- push new array to the stack
    createtable (length cs') 0
    -- call callbacks and fill array with return values
    iter cs'
    return 1
 where
  iter [] = return ()
  iter (c : rest) = do
    getglobal2 "table.insert"
    pushvalue (-2)
    pushinteger (fromIntegral c)
    gettable registryindex
    -- call the callback
    pcall 0 1 0
    -- call table.insert
    pcall 2 0 0
    iter rest

-- | Reset callback queue and remove Lua functions from registry to enable
-- garbage collection.
resetLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO CInt
resetLuaCallbacks cs l = do
  cs' <- readIORef cs
  runLuaWith l $ mapM_ (unref registryindex) cs'
  writeIORef cs []
  return 0
