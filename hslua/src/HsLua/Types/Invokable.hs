{-|
Module      : HsLua.Types.Invokable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call Lua functions from Haskell.
-}
module HsLua.Types.Invokable
  ( Invokable (..)
  , invoke
  ) where

import HsLua.Core as Lua
import HsLua.Types.Peekable
import HsLua.Types.Pushable
import HsLua.Util (getglobal', popValue)

-- | Helper class used to make Lua functions useable from Haskell.
class Invokable a where
  addArg :: String -> Lua () -> NumArgs -> a

instance Peekable a => Invokable (Lua a) where
  addArg fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1
    popValue

instance (Pushable a, Invokable b) => Invokable (a -> b) where
  addArg fnName pushArgs nargs x =
    addArg fnName (pushArgs *> push x) (nargs + 1)

-- | Invoke a Lua function. Use as:
--
-- > v <- invoke "proc" "abc" (1::Int) (5.0::Double)
invoke :: (Invokable a) => String -> a
invoke f = addArg f (return ()) 0
