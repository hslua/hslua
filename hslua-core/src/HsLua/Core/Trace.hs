{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Core.Trace
Copyright   : © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Helper functions to call Lua functions with tracebacks.
-}
module HsLua.Core.Trace
  ( pcallTrace
  , callTrace
  , dofileTrace
  , dostringTrace
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types
import HsLua.Core.Auxiliary (loadfile, loadstring, tostring', traceback)
import HsLua.Core.Error (Exception, LuaError, throwErrorAsException)
import HsLua.Core.Primary (gettop, insert, pcall, pushcfunction, remove)
import HsLua.Core.Run (runWith)
import HsLua.Core.Types
  ( CFunction, LuaE, NumArgs (..), NumResults (..), PreCFunction
  , Status (OK), State (..), multret )

-- | Like @'pcall'@, but sets an appropriate message handler function,
-- thereby adding a stack traceback if an error occurs.
pcallTrace :: NumArgs -> NumResults -> LuaE e Status
pcallTrace nargs@(NumArgs nargsint) nres = do
  curtop <- gettop
  let base = curtop - fromIntegral nargsint -- function index
  pushcfunction hsluaL_msghandler_ptr
  insert base  -- insert msghandler below function
  status' <- pcall nargs nres (Just base)
  remove base
  return status'

-- | Like @'call'@, but adds a traceback if an error occurs.
callTrace :: LuaError e => NumArgs -> NumResults -> LuaE e ()
callTrace nargs nres = pcallTrace nargs nres >>= \case
  OK -> pure ()
  _  -> throwErrorAsException

-- | Run the given file as a Lua program, while also adding a
-- traceback to the error message if an error occurs.
dofileTrace :: FilePath -> LuaE e Status
dofileTrace fp = loadfile fp >>= \case
  OK -> pcallTrace 0 multret
  s  -> pure s

dostringTrace :: ByteString -> LuaE e Status
dostringTrace s = loadstring s >>= \case
  OK  -> pcallTrace 0 multret
  err -> pure err

-- | Helper function used as message handler if the function given to
-- pcall fails.
hsluaL_msghandler :: State -> IO NumResults
hsluaL_msghandler l = runWith l $ do
  msg <- tostring' @Exception 1
  traceback l (Just msg) 2
  pure (NumResults 1)

-- Turn message handler into a CFunction by exporting it, then importing
-- at pointer to it.
foreign export ccall hsluaL_msghandler :: PreCFunction
foreign import ccall "&hsluaL_msghandler"
  hsluaL_msghandler_ptr:: CFunction
