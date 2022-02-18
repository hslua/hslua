{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : HsLua.Marshalling.Userdata
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Convenience functions to use Haskell values as Lua userdata.
-}
module HsLua.Marshalling.Userdata
  ( pushIterator
  ) where

import Control.Monad (void)
import HsLua.Core as Lua

-- | Pushes three values to the stack that can be used in a generic for
-- loop to lazily iterate over all values in the list. Keeps the
-- remaining list in a userdata state.
--
-- If the values pusher function returns @'NumResults' 0@ for a list
-- item, then this item will be skipped and the values for the next item
-- will be pushed.
pushIterator :: forall a e. LuaError e
             => (a -> LuaE e NumResults)  -- ^ pusher for the values
             -> [a]                       -- ^ list to iterate over lazily
             -> LuaE e NumResults
pushIterator pushValues xs = do
  -- push initial state
  pushHaskellFunction nextItem
  pushInitialState
  pushnil
  return (NumResults 3)
  where
    nextItem :: LuaE e NumResults
    nextItem = do
      props <- fromuserdata @[a] (nthBottom 1) statename
      case props of
        Nothing -> failLua
          "Error in iterator: could not retrieve iterator state."
        Just [] -> 2 <$ (pushnil *> pushnil)  -- end loop
        Just (y:ys) -> do
          success <- putuserdata @[a] (nthBottom 1) statename ys
          if not success
            then failLua "Error in iterator: could not update iterator state."
            else pushValues y >>= \case
              0 -> nextItem  -- keep going if nothing was pushed
              n -> return n

    statename :: Name
    statename = "HsLua iterator state"

    pushInitialState :: LuaE e ()
    pushInitialState = do
      newhsuserdatauv @[a] xs 0
      void (newudmetatable statename)
      setmetatable (nth 2)
