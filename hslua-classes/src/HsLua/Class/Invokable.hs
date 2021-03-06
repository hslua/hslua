{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-|
Module      : HsLua.Class.Invokable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call Lua functions from Haskell.
-}
module HsLua.Class.Invokable
  ( Invokable (..)
  , invoke
  ) where

import HsLua.Core as Lua
import HsLua.Class.Peekable
import HsLua.Class.Pushable
import HsLua.Class.Util (popValue)
import HsLua.Util (getglobal')

-- | Helper class used to make Lua functions useable from Haskell.
class PeekError e => Invokable e a where
  addArg :: String -> LuaE e () -> NumArgs -> a

instance (PeekError e, Peekable a) => Invokable e (LuaE e a) where
  addArg fnName pushArgs nargs = do
    getglobal' fnName
    pushArgs
    call nargs 1
    popValue

instance (Pushable a, PeekError e, Invokable e b) => Invokable e (a -> b) where
  addArg fnName pushArgs nargs x =
    addArg fnName (pushArgs *> push x) (nargs + 1)

-- | Invoke a Lua function. Use as:
--
-- > v <- invoke "proc" "abc" (1::Int) (5.0::Double)
invoke :: forall e a. Invokable e a => String -> a
invoke fname = addArg @e fname (return ()) 0
