{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module HsLua.ObjectOrientation.ListType
  ( ListSpec
    -- * Semi-internal
  , lazyListStateName
  , lazylisteval
  , setList
  ) where

import Control.Monad ((<$!>), forM_, void)
import HsLua.Core
import HsLua.Marshalling

-- | Pair of pairs, describing how a type can be used as a Lua list. The
-- first pair describes how to push the list items, and how the list is
-- extracted from the type; the second pair contains a method to
-- retrieve list items, and defines how the list is used to create an
-- updated value.
type ListSpec e a itemtype =
  ( (Pusher e itemtype, a -> [itemtype])
  , (Peeker e itemtype, a -> [itemtype] -> a)
  )

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
setList :: forall itemtype e a. LuaError e
        => ListSpec e a itemtype -> a
        -> Peek e a
setList (_pushspec, (peekItem, updateList)) x = (x `updateList`) <$!> do
  liftLua (getfield top "__lazylistindex") >>= \case
    TypeBoolean -> do
      -- list had been fully evaluated
      liftLua $ pop 1
      peekList peekItem top
    _ -> do
      let getLazyList = do
            liftLua (getfield top "__lazylist") >>= \case
              TypeUserdata -> pure ()
              _ -> failPeek "unevaled items of lazy list cannot be peeked"
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
