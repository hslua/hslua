{-|
Module      : Foreign.Lua.Types
Copyright   : © 2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Types for working with Lua.
-}
module Foreign.Lua.Types
  ( module Foreign.Lua.Types.Peekable
  , module Foreign.Lua.Types.Pushable
  ) where

import Foreign.Lua.Types.Peekable
import Foreign.Lua.Types.Pushable
