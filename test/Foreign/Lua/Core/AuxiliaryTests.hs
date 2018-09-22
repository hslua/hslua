{-| Tests for the auxiliary library.
-}
module Foreign.Lua.Core.AuxiliaryTests (tests) where

import Test.HsLua.Util ( (=:))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Foreign.Lua as Lua

-- | Specifications for Attributes parsing functions.
tests :: TestTree
tests = testGroup "Auxiliary"
  [ "loadedTable" =: ("_LOADED" @=? Lua.loadedTableRegistryField)
  , "preloadTable" =: ("_PRELOAD" @=? Lua.preloadTableRegistryField)
  ]
