
-- An example hslua program that demonstrates providing Haskell callbacks
-- to Lua and getting Lua callbacks from Haskell.

import Scripting.Lua as Lua
import Data.IORef

main:: IO ()
main = do
    callbacks <- newIORef []
    l <- newstate
    openlibs l
    registerhsfunction l "addLuaCallbacks" (addLuaCallbacks callbacks l)
    registerhsfunction l "callLuaCallbacks" (callLuaCallbacks callbacks l)
    registerhsfunction l "resetLuaCallbacks" (resetLuaCallbacks callbacks l)
    loadfile l "callbacks.lua"
    call l 0 0
    close l

type LuaFunRef = Int

-- | Get Lua callbacks as argument to later call them in order.
-- Successive calls to this function without calling `resetLuaCallbacks`
-- adds more callbacks to the queue.
-- (I know lists are not the best functional queue implementations ...)
addLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO Int
addLuaCallbacks cs l = do
    -- number of arguments passed to this function
    args <- gettop l
    -- make sure arguments are functions
    as <- checkArgs args
    if not as
      then do
        -- TODO: how to handle errors?
        return 0
      else do
        addCallbacks 1 args
        return 0
  where
    checkArgs :: Int -> IO Bool
    checkArgs 0 = return True
    checkArgs n = do
      ty <- ltype l n
      if ty == TFUNCTION
        then checkArgs (n-1)
        else return False

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
-- TODO: as an exercise, return values returned by callbacks
callLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO ()
callLuaCallbacks cs l = do
    cs' <- readIORef cs
    iter cs'
  where
    iter [] = return ()
    iter (c : rest) = do
      pushinteger l (fromIntegral c)
      gettable l registryindex
      call l 0 0
      iter rest

-- | Reset callback queue and remove Lua functions from registry to enable
-- garbage collection.
resetLuaCallbacks :: IORef [LuaFunRef] -> LuaState -> IO ()
resetLuaCallbacks cs l = do
    cs' <- readIORef cs
    mapM_ (unref l registryindex) cs'
    writeIORef cs []
