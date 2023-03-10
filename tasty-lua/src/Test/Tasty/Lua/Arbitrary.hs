{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-|
Module      : Test.Tasty.Lua.Arbitrary
Copyright   : © 2019-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Generators for arbitrary Lua values.
-}
module Test.Tasty.Lua.Arbitrary
  ( registerArbitrary
  , registerDefaultGenerators
  , pushArbitraryTable
  )
where

import HsLua.Core
import HsLua.Marshalling
import Lua.Arbitrary ()
import Test.QuickCheck (Arbitrary (..), generate, vectorOf)

-- | Register a Lua value generator.
registerArbitrary :: forall a e. (Arbitrary a, LuaError e)
                  => Name
                  -> Pusher e a
                  -> Peeker e a
                  -> LuaE e ()
registerArbitrary name push peek = do
  pushArbitraryTable
  pushName name
  newtable
  pushName "generator"
  pushHaskellFunction $ do
    samples <- liftIO (generate $ vectorOf 30 (arbitrary @a))
    pushIterator (\x -> NumResults 1 <$ push x) samples
  rawset (nth 3)
  pushName "shrink"
  pushHaskellFunction $
    runPeeker peek (nthBottom 1) >>= \case
      Success x -> do
        pushList push (shrink x)
        pure (NumResults 1)
      _ -> pure (NumResults 0)
  rawset (nth 3)
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
  registerArbitrary "boolean" pushboolean peekBool
  registerArbitrary "integer" pushinteger peekIntegral
  registerArbitrary "number"  pushnumber  peekRealFloat
  registerArbitrary "string"  pushString  peekString
