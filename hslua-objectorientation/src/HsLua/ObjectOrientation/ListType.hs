{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module HsLua.ObjectOrientation.ListType
  ( UDTypeWithList
  , ListSpec
  , listExtension
  ) where

import Control.Monad ((<$!>), forM_, void)
import HsLua.Core as Lua
import HsLua.Marshalling
import HsLua.ObjectOrientation.Generic

-- | Userdata type that (also) behaves like a list.
type UDTypeWithList e fn a itemtype =
  UDTypeGeneric e fn a
{-# DEPRECATED UDTypeWithList "Use UDTypeGeneric instead" #-}

-- | Pair of pairs, describing how a type can be used as a Lua list. The
-- first pair describes how to push the list items, and how the list is
-- extracted from the type; the second pair contains a method to
-- retrieve list items, and defines how the list is used to create an
-- updated value.
type ListSpec e a itemtype =
  ( (Pusher e itemtype, a -> [itemtype])
  , (Peeker e itemtype, a -> [itemtype] -> a)
  )

listExtension
  :: LuaError e
  => ListSpec e a itemtype
  -> UDTypeHooks e fn a
listExtension ((pushItem, toList), (peekItem, updateList)) =
  UDTypeHooks
  { hookMetatableSetup = do
      pushName "lazylisteval"
      pushHaskellFunction (lazylisteval pushItem)
      rawset (nth 3)

  , hookPeekUD = \x idx ->
    (`lastly` pop 1) $ liftLua (getiuservalue idx 1) >>= \case
      TypeTable -> setList peekItem updateList x
      _other    -> pure x

  , hookPushUD = \x -> do
      newtable
      pushName "__lazylist"
      newhsuserdatauv (toList x) 1
      void (newudmetatable lazyListStateName)
      setmetatable (nth 2)
      rawset (nth 3)
      void (setiuservalue (nth 2) 1)

  , hookUservalues = 1
  }


-- | Evaluate part of a lazy list. Takes the following arguments, in
-- this order:
--
-- 1. userdata wrapping the unevalled part of the lazy list
-- 2. index of the last evaluated element
-- 3. index of the requested element
-- 4. the caching table
lazylisteval :: forall itemtype e. LuaError e
             => Pusher e itemtype -> LuaE e NumResults
lazylisteval pushItem = do
  munevaled <- fromuserdata @[itemtype] (nthBottom 1) lazyListStateName
  mcurindex <- tointeger (nthBottom 2)
  mnewindex <- tointeger (nthBottom 3)
  case (munevaled, mcurindex, mnewindex) of
    (Just unevaled, Just curindex, Just newindex) -> do
      let numElems = fromIntegral $ max (newindex - curindex) 0
          (as, rest) = splitAt numElems unevaled
      if null rest
        then do
          -- no more elements in list; unset variable
          pushName "__lazylistindex"
          pushBool False
          rawset (nthBottom 4)
        else do
          -- put back remaining unevalled list
          void $ putuserdata @[itemtype] (nthBottom 1) lazyListStateName rest
          pushName "__lazylistindex"
          pushinteger (curindex + fromIntegral (length as))
          rawset (nthBottom 4)
      -- push evaluated elements
      forM_ (zip [(curindex + 1)..] as) $ \(i, a) -> do
        pushItem a
        rawseti (nthBottom 4) i
      return (NumResults 0)
    _ -> pure (NumResults 0)

-- | Name of the metatable used for unevaluated lazy list rema
lazyListStateName :: Name
lazyListStateName = "HsLua unevalled lazy list"

-- | Gets a list from a uservalue table and sets it on the given value.
-- Expects the uservalue (i.e., caching) table to be at the top of the
-- stack.
setList :: forall itemtype a e. LuaError e
        => Peeker e itemtype
        -> (a -> [itemtype] -> a)
        -> a
        -> Peek e a
setList peekItem updateList x = (x `updateList`) <$!> do
  liftLua (getfield top "__lazylistindex") >>= \case
    TypeBoolean -> do
      -- list had been fully evaluated
      liftLua $ pop 1
      peekList peekItem top
    _ -> do
      let getLazyList = do
            liftLua (getfield top "__lazylist") >>= \case
              TypeUserdata -> pure ()
              otherType -> do
                tyname <- liftLua $ typename otherType
                failPeek $
                  "unevaled items of lazy list cannot be peeked: got " <>
                  tyname
            (`lastly` pop 1) $ reportValueOnFailure
              lazyListStateName
              (\idx -> fromuserdata @[itemtype] idx lazyListStateName)
              top
      mlastIndex <- liftLua (tointeger top <* pop 1)
      let itemsAfter = case mlastIndex of
            Nothing -> const getLazyList
            Just lastIndex -> \i ->
              if i <= lastIndex
              then liftLua (rawgeti top i) >>= \case
                TypeNil -> [] <$ liftLua (pop 1)
                _ -> do
                  y <- peekItem top `lastly` pop 1
                  (y:) <$!> itemsAfter (i + 1)
              else getLazyList
      itemsAfter 1
