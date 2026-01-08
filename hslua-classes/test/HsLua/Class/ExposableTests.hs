{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : HsLua.Class.ExposableTests
Copyright   : © 2017-2026 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb@hslua.org>

Tests that Haskell functions can be exposed to and called from Lua.
-}
module HsLua.Class.ExposableTests (tests) where

import HsLua.Core (Lua)
import HsLua.Class.Exposable as Lua
import HsLua.Class.Peekable as Lua
import Test.Tasty.HsLua ( (=:), pushLuaExpr, shouldBeErrorMessageOf
                       , shouldBeResultOf )
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests =
  let integerOperation :: Lua.Integer -> Lua.Integer -> Lua Lua.Integer
      integerOperation i1 i2 =
        let (j1, j2) = (fromIntegral i1, fromIntegral i2)
        in return $ fromIntegral (product [1..j1] `mod` j2 :: Prelude.Integer)
  in testGroup "Exposable"
  [ "push Haskell function to Lua" =:
    (28 :: Lua.Integer) `shouldBeResultOf` do
      let add :: Lua Lua.Integer
          add = do
            i1 <- Lua.peek (-1)
            i2 <- Lua.peek (-2)
            return (i1 + i2)
      Lua.registerHaskellFunction "add" add
      Lua.loadstring "return add(23, 5)" *> Lua.call 0 1
      Lua.peek Lua.top <* Lua.pop 1

  , "push multi-argument Haskell function to Lua" =:
    (0 :: Lua.Integer) `shouldBeResultOf` do
      Lua.registerHaskellFunction "integerOp" integerOperation
      Lua.loadstring "return integerOp(23, 42)" *> Lua.call 0 1
      Lua.peek (-1) <* Lua.pop 1

  , "argument type errors are propagated" =:
     ("integer expected, got boolean" ++
      "\n\twhile retrieving argument 2" ++
      "\n\twhile executing function call") `shouldBeErrorMessageOf` do
          Lua.registerHaskellFunction "integerOp" integerOperation
          pushLuaExpr "integerOp(23, true)"

  , "Error in Haskell function is converted into Lua error" =:
    (False, "foo") `shouldBeResultOf` do
      Lua.openlibs
      Lua.pushAsHaskellFunction (Lua.failLua "foo" :: Lua ())
      Lua.setglobal "throw_foo"
      Lua.loadstring "return pcall(throw_foo)" *> Lua.call 0 2
      (,) <$> Lua.peek (Lua.nth 2) <*> Lua.peek @String (Lua.nth 1)
  ]
