
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      :  HsLua.Core.RunTests
Copyright   :  © 2017-2022 Albert Krewinkel
License     :  MIT

Maintainer  :  Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   :  stable
Portability :  portable

Instances for QuickCheck's Arbitrary.
-}
module Test.HsLua.Arbitrary () where

import HsLua.Core (Type)
import Test.QuickCheck (Arbitrary(arbitrary))
import qualified HsLua.Core as Lua
import qualified Test.QuickCheck as QC

instance Arbitrary Lua.Integer where
  arbitrary = QC.arbitrarySizedIntegral

instance Arbitrary Lua.Number where
  arbitrary = Lua.Number <$> arbitrary

instance Arbitrary Type where
  arbitrary = QC.arbitraryBoundedEnum
