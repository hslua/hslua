{-|
Module      : HsLua.PackagingTests
Copyright   : © 2020-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Test packaging
-}
module HsLua.PackagingTests (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified HsLua.Packaging.DocumentationTests
import qualified HsLua.Packaging.FunctionTests
import qualified HsLua.Packaging.ModuleTests
import qualified HsLua.Packaging.RenderingTests
import qualified HsLua.Packaging.UDTypeTests

-- | Tests for package creation.
tests :: TestTree
tests = testGroup "Packaging"
  [ HsLua.Packaging.FunctionTests.tests
  , HsLua.Packaging.ModuleTests.tests
  , HsLua.Packaging.RenderingTests.tests
  , HsLua.Packaging.UDTypeTests.tests
  , HsLua.Packaging.DocumentationTests.tests
  ]
