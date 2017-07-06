{-
Copyright © 2017 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-|
Module      : Foreign.Lua.Types.Core
Copyright   : © 2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : GeneralizedNewtypeDeriving

The core Lua types, including mappings of Lua types to Haskell.
-}
module Foreign.Lua.Types.Core
  ( Lua (..)
  , runLuaWith
  , liftIO
  , liftLua
  , liftLua1
  ) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadIO, ask, liftIO)
import Foreign.Lua.Api.Types (LuaState)

-- | Lua computation
newtype Lua a = Lua { unLua :: ReaderT LuaState IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadReader LuaState
    , MonadThrow
    )

-- | Turn a function of typ @LuaState -> IO a@ into a monadic lua operation.
liftLua :: (LuaState -> IO a) -> Lua a
liftLua f = luaState >>= liftIO . f

-- | Turn a function of typ @LuaState -> a -> IO b@ into a monadic lua operation.
liftLua1 :: (LuaState -> a -> IO b) -> a -> Lua b
liftLua1 f x = liftLua $ \l -> f l x

-- | Get the lua state of this lua computation.
luaState :: Lua LuaState
luaState = ask

-- | Run lua computation with custom lua state.
runLuaWith :: LuaState -> Lua a -> IO a
runLuaWith l s = runReaderT (unLua s) l
