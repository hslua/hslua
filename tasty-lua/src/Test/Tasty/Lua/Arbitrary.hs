{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : Test.Tasty.Lua.Arbitrary
Copyright   : © 2019–2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>

Generators for arbitrary Lua values.
-}
module Test.Tasty.Lua.Arbitrary
  ( registerArbitrary
  , registerDefaultGenerators
  , pushArbitraryTable
  )
where

import HsLua.Core
  ( LuaE, LuaError, Name
  , liftIO, newmetatable, nth, pop, pushHaskellFunction, pushboolean
  , pushinteger, pushnumber, pushvalue, rawset, setmetatable, top )
import HsLua.Marshalling ( Pusher, pushIterator, pushName, pushString )
import Lua.Arbitrary ()
import Test.QuickCheck (Arbitrary (..), generate, vectorOf)

-- | Register a Lua value generator.
registerArbitrary :: forall a e. (Arbitrary a, LuaError e)
                  => Name -> Pusher e a -> LuaE e ()
registerArbitrary name push = do
  pushArbitraryTable
  pushName name
  pushHaskellFunction $ do
    samples <- liftIO (generate $ vectorOf 30 (arbitrary @a))
    pushIterator (\x -> 1 <$ push x) samples
  rawset (nth 3)
  pop 1  -- remove `tasty.arbitrary` table

-- | Pushes the table holding all arbitrary generators to the stack.
pushArbitraryTable :: LuaE e ()
pushArbitraryTable =
  newmetatable "tasty.arbitrary" >>= \case
    False ->    -- table exists
      pure ()
    True  -> do -- table created
      -- make table it's own metatable
      pushvalue top
      setmetatable (nth 2)

registerDefaultGenerators :: LuaError e => LuaE e ()
registerDefaultGenerators = do
  registerArbitrary "boolean" pushboolean
  registerArbitrary "integer" pushinteger
  registerArbitrary "number" pushnumber
  registerArbitrary "string" pushString
