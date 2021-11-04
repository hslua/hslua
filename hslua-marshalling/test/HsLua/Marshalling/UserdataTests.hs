{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Marshalling.UserdataTests
Copyright   : © 2018-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tests that any data type can be pushed to Lua as userdata.
-}
module HsLua.Marshalling.UserdataTests (tests) where

import Control.Monad (when)
import HsLua.Marshalling.Userdata
import Test.Tasty.HsLua ( (=:), shouldBeResultOf )
import Test.Tasty (TestTree, testGroup)

import qualified HsLua.Core as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Userdata"
  [ testGroup "pushIterator"
    [ "iterate over list" =:
      Just "0,1,1,2,3,5,8,13,21" `shouldBeResultOf` do
        let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
        Lua.openlibs
        Lua.pushHaskellFunction $
          pushIterator (\n -> 1 <$ Lua.pushinteger n) (take 9 fibs)
        Lua.setglobal "fibs"
        stat <- Lua.dostring $ mconcat
          [ "local acc = {}\n"
          , "for n in fibs() do\n"
          , "  table.insert(acc, n)\n"
          , "end\n"
          , "return table.concat(acc, ',')\n"
          ]
        when (stat /= Lua.OK) Lua.throwErrorAsException
        Lua.tostring Lua.top
    , "skip entry if value pusher returned 0" =:
      Just "1,3,4" `shouldBeResultOf` do
        let pushNoTwo 2 = return 0
            pushNoTwo i = 1 <$ Lua.pushinteger i
        Lua.openlibs
        Lua.pushHaskellFunction $ pushIterator pushNoTwo [1..4]
        Lua.setglobal "skip"
        stat <- Lua.dostring $ mconcat
          [ "local acc = {}\n"
          , "for n in skip() do table.insert(acc, n) end\n"
          , "return table.concat(acc, ',')\n"
          ]
        when (stat /= Lua.OK) Lua.throwErrorAsException
        Lua.tostring Lua.top
    ]
  ]
