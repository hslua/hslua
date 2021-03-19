{-|
Module      : HsLua.Packaging
Copyright   : © 2019-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Tools to create Lua modules.
-}
module HsLua.Packaging
  ( -- * Documented functions
    module HsLua.Packaging.Function
    -- * Documented Modules
  , module HsLua.Packaging.Module
  ) where

import HsLua.Packaging.Function hiding (render)
import HsLua.Packaging.Module
