{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-|
Module      : HsLua.Class.Exposable
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables

Call haskell functions from Lua, and vice versa.
-}
module HsLua.Class.Exposable
  ( Exposable (..)
  , toHaskellFunction
  , registerHaskellFunction
  ) where

import HsLua.Core as Lua
import HsLua.Class.Peekable (Peekable (peek), PeekError (..), inContext)
import HsLua.Class.Pushable (Pushable (push))
import qualified Control.Monad.Catch as Catch

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

-- | Operations and functions that can be pushed to the Lua stack. This
-- is a helper function not intended to be used directly. Use the
-- @'toHaskellFunction'@ wrapper instead.
class PeekError e => Exposable e a where
  -- | Helper function, called by @'toHaskellFunction'@. Should do a
  -- partial application of the argument at the given index to the
  -- underlying function. Recurses if necessary, causing further partial
  -- applications until the operation is a easily exposable to Lua.
  partialApply :: StackIndex -> a -> LuaE e NumResults

instance {-# OVERLAPPING #-} PeekError e =>
         Exposable e (HaskellFunction e) where
  partialApply _ = id

instance (PeekError e, Pushable a) => Exposable e (LuaE e a) where
  partialApply _narg x = 1 <$ (x >>= push)

instance (Peekable a, Exposable e b) => Exposable e (a -> b) where
  partialApply narg f = getArg >>= partialApply (narg + 1) . f
    where
      getArg = inContext errorPrefix (peek narg)
      errorPrefix = "could not read argument " ++
                    show (fromStackIndex narg) ++ ": "

-- | Convert a Haskell function to a function type directly exposable to
-- Lua. Any Haskell function can be converted provided that:
--
--   * all arguments are instances of @'Peekable'@
--   * return type is @Lua a@, where @a@ is an instance of
--     @'Pushable'@
--
-- Any @'Lua.Exception'@ will be converted to a string and returned
-- as Lua error.
--
-- /Important/: this does __not__ catch exceptions other than
-- @'Lua.Exception'@; exception handling must be done by the converted
-- Haskell function. Failure to do so will cause the program to crash.
--
-- E.g., the following code could be used to handle an Exception
-- of type FooException, if that type is an instance of
-- 'Control.Monad.Catch.MonadCatch' and 'Pushable':
--
-- > toHaskellFunction (myFun `catchM` (\e -> raiseError (e :: FooException)))
--
toHaskellFunction :: forall e a. Exposable e a => a -> HaskellFunction e
toHaskellFunction a = do
  let ctx = "Error during function call:"
  -- Lua.exceptionToError errConv . Lua.addContextToException errConv ctx $
  partialApply 1 a `Catch.catch` \(err :: e) ->
    Catch.throwM (exceptionFromMessage ctx <> err)


-- | Imports a Haskell function and registers it at global name.
registerHaskellFunction :: Exposable e a
                        => String -> a -> LuaE e ()
registerHaskellFunction n f = do
  pushHaskellFunction $ toHaskellFunction f
  setglobal n
