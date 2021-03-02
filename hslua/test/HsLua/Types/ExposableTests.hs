{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Types.ExposableTests
Copyright   : © 2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that Haskell functions can be exposed to and called from Lua.
-}
module HsLua.Types.ExposableTests (tests) where

import Control.Monad (forM_)
import HsLua (Lua)
import Test.Tasty.HsLua ( (=:), pushLuaExpr, shouldBeErrorMessageOf
                       , shouldBeResultOf )
import Test.Tasty (TestTree, testGroup)

import qualified HsLua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests =
  let integerOperation :: Lua.Integer -> Lua.Integer -> Lua Lua.Integer
      integerOperation i1 i2 =
        let (j1, j2) = (fromIntegral i1, fromIntegral i2)
        in return $ fromIntegral (product [1..j1] `mod` j2 :: Prelude.Integer)
  in testGroup "Exposable"
  [ "push haskell function to lua" =:
    (28 :: Lua.Integer) `shouldBeResultOf` do
      let add :: Lua Lua.Integer
          add = do
            i1 <- Lua.peek (-1)
            i2 <- Lua.peek (-2)
            return (i1 + i2)
      Lua.registerHaskellFunction "add" add
      Lua.loadstring "return add(23, 5)" *> Lua.call 0 1
      Lua.peek Lua.top <* Lua.pop 1

  , "push multi-argument haskell function to lua" =:
    (0 :: Lua.Integer) `shouldBeResultOf` do
      Lua.registerHaskellFunction "integerOp" integerOperation
      Lua.loadstring "return integerOp(23, 42)" *> Lua.call 0 1
      Lua.peek (-1) <* Lua.pop 1

  , "argument type errors are propagated" =:
     ("Error during function call: could not read argument 2: "
      <> "expected integer, got 'true' (boolean)") `shouldBeErrorMessageOf` do
          Lua.registerHaskellFunction "integerOp" integerOperation
          pushLuaExpr "integerOp(23, true)"

  , "Haskell functions are converted to C functions" =:
    (100 :: Lua.Integer) `shouldBeResultOf` do
      Lua.pushHaskellFunction integerOperation
      Lua.pushinteger 71
      Lua.pushinteger 107
      Lua.call 2 1
      Lua.peek Lua.top <* Lua.pop 1

  , "Error in Haskell function is converted into Lua error" =:
    (False, "Error during function call: foo" :: String) `shouldBeResultOf` do
      Lua.openlibs
      Lua.pushHaskellFunction (Lua.throwException "foo" :: Lua ())
      Lua.setglobal "throw_foo"
      Lua.loadstring "return pcall(throw_foo)" *> Lua.call 0 2
      (,) <$> Lua.peek (Lua.nth 2) <*> Lua.peek (Lua.nth 1)

  -- The following test case will hang if there's a problem with garbage
  -- collection.
  , "function garbage collection" =:
    () `shouldBeResultOf` do
      let pushAndPopAdder n = do
            let fn :: Lua.Integer -> Lua Lua.Integer
                fn x = return (x + n)
            Lua.pushHaskellFunction fn
            Lua.pop 1
      forM_ [1..5000::Lua.Integer] pushAndPopAdder
      () <$ Lua.gc Lua.GCCOLLECT 0
  ]
