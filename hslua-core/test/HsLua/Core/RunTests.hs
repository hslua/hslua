{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-|
Module      :  HsLua.Core.RunTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Tests for different convenience functions to run Lua operations.
-}
module HsLua.Core.RunTests (tests) where

import Data.Either (isLeft, isRight)
import HsLua.Core as Lua
import Test.Tasty.HsLua ((=:), shouldHoldForResultOf)
import Test.Tasty (TestTree, testGroup)

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Run"
  [ testGroup "runEither"
    [ "Lua errors are caught" =:
      isLeft `shouldHoldForResultOf`
      liftIO (runEither (failLua "failing" :: Lua Bool))

    , "error-less code gives 'Right'" =:
      isRight `shouldHoldForResultOf`
        liftIO (runEither @Lua.Exception (pushboolean True *> toboolean top))
    ]
  ]
