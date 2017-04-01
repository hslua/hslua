{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017 Albert Krewinkel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Foreign.Lua
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel,
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface, FlexibleInstances,
              ScopedTypeVariables

Lua bindings and helpers for haskell.
-}
module Foreign.Lua
  ( module Foreign.Lua
  , module Foreign.Lua.Types
  , module Foreign.Lua.Bindings
  ) where

import Prelude hiding ( compare, concat )

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ( (<$>) )
#endif
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as L
import Data.Maybe
import Foreign.C
import Foreign.Lua.Bindings
import Foreign.Lua.Types
import Foreign.Ptr
import Foreign.StablePtr
import qualified Foreign.Storable as F
import qualified Prelude

#if LUA_VERSION_NUMBER >= 502
{-# DEPRECATED strlen "Use rawlen instead." #-}
#else
{-# DEPRECATED strlen "Use objlen instead." #-}
#endif
-- | Compatibility alias for objlen
strlen :: LuaState -> StackIndex -> IO Int
strlen = objlen

-- | This is a convenience function to implement error propagation convention
-- described in [Error handling in hslua](#g:1). hslua doesn't implement
-- `lua_error` function from Lua C API because it's never safe to use. (see
-- [Error handling in hslua](#g:1) for details)
lerror :: LuaState -> IO Int
lerror l = do
    getglobal l "_HASKELLERR"
    insert l (-2)
    return 2

-- | Push a list to Lua stack as a Lua array.
pushlist :: StackValue a => LuaState -> [a] -> IO ()
pushlist l list = do
    newtable l
    forM_ (zip [1..] list) $ \(idx, val) -> do
      push l val
      rawseti l (-2) idx

-- | Try to convert Lua array at given index to Haskell list.
tolist :: StackValue a => LuaState -> StackIndex -> IO (Maybe [a])
tolist l n = do
    len <- objlen l n
    iter [1..len]
  where
    iter [] = return $ Just []
    iter (i : is) = do
      rawgeti l n i
      ret <- peek l (-1)
      pop l 1
      case ret of
        Nothing  -> return Nothing
        Just val -> do
          rest <- iter is
          return $ case rest of
                     Nothing -> Nothing
                     Just vals -> Just (val : vals)

-- | A value that can be pushed and poped from the Lua stack.
-- All instances are natural, except following:
--
--  * @LuaState@ push ignores its argument, pushes current state
--
--  * @()@ push ignores its argument, just pushes nil
--
--  * @Ptr ()@ pushes light user data, peek checks for lightuserdata or userdata
--
--  * See "A note about integer functions" for integer functions.
class StackValue a where
    -- | Pushes a value onto Lua stack, casting it into meaningfully nearest Lua type.
    push :: LuaState -> a -> IO ()
    -- | Check if at index @n@ there is a convertible Lua value and if so return it
    -- wrapped in @Just@. Return @Nothing@ otherwise.
    peek :: LuaState -> StackIndex -> IO (Maybe a)
    -- | Lua type id code of the vaule expected. Parameter is unused.
    valuetype :: a -> LTYPE

maybepeek :: l -> n -> (l -> n -> IO Bool) -> (l -> n -> IO r) -> IO (Maybe r)
maybepeek l n test peekfn = do
    v <- test l n
    if v
      then liftM Just (peekfn l n)
      else return Nothing

instance StackValue LuaInteger where
    push l x = pushinteger l x
    peek l n = maybepeek l n isnumber tointeger
    valuetype _ = TNUMBER

instance StackValue LuaNumber where
    push l x = pushnumber l x
    peek l n = maybepeek l n isnumber tonumber
    valuetype _ = TNUMBER

instance StackValue Int where
    push l x = pushinteger l (fromIntegral x)
    peek l n = maybepeek l n isnumber (\l' n' -> liftM fromIntegral (tointeger l' n'))
    valuetype _ = TNUMBER

instance StackValue B.ByteString where
    push l x = pushstring l x
    peek l n = maybepeek l n isstring tostring
    valuetype _ = TSTRING

instance StackValue a => StackValue [a] where
    push l x = pushlist l x
    peek l n = tolist l n
    valuetype _ = TTABLE

instance StackValue Bool where
    push l x = pushboolean l x
    peek l n = maybepeek l n isboolean toboolean
    valuetype _ = TBOOLEAN

instance StackValue (FunPtr LuaCFunction) where
    push l x = pushcfunction l x
    peek l n = maybepeek l n iscfunction tocfunction
    valuetype _ = TFUNCTION

-- watch out for the asymetry here
instance StackValue (Ptr a) where
    push l x = pushlightuserdata l x
    peek l n = maybepeek l n isuserdata touserdata
    valuetype _ = TUSERDATA

-- watch out for push here
instance StackValue LuaState where
    push l _ = pushthread l >> return ()
    peek l n = maybepeek l n isthread tothread
    valuetype _ = TTHREAD

instance StackValue () where
    push l _ = pushnil l
    peek l n = maybepeek l n isnil (\_l _n -> return ())
    valuetype _ = TNIL

-- | Like @getglobal@, but knows about packages. e. g.
--
-- > getglobal l "math.sin"
--
-- returns correct result
getglobal2 :: LuaState -> String -> IO ()
getglobal2 l n = do
    getglobal l x
    mapM_ dotable xs
  where
    (x : xs)  = splitdot n
    splitdot  = filter (/= ".") . L.groupBy (\a b -> a /= '.' && b /= '.')
    dotable a = getfield l (-1) a >> gettop l >>= \i -> remove l (i - 1)


typenameindex :: LuaState -> StackIndex -> IO String
typenameindex l n = ltype l n >>= typename l

class LuaImport a where
    luaimport' :: StackIndex -> a -> LuaCFunction
    luaimportargerror :: StackIndex -> String -> a -> LuaCFunction

instance (StackValue a) => LuaImport (IO a) where
    luaimportargerror _n msg _x l = do
      -- TODO: maybe improve the error message
      pushstring l (BC.pack msg)
      fromIntegral <$> lerror l
    luaimport' _narg x l = x >>= push l >> return 1

instance (StackValue a, LuaImport b) => LuaImport (a -> b) where
    luaimportargerror n msg x l = luaimportargerror n msg (x undefined) l
    luaimport' narg x l = do
      arg <- peek l narg
      case arg of
        Just v -> luaimport' (narg+1) (x v) l
        Nothing -> do
          t <- ltype l narg
          expected <- typename l (valuetype (fromJust arg))
          got <- typename l t
          luaimportargerror narg
            (Prelude.concat [ "argument ", show (fromStackIndex narg)
                            , " of Haskell function: ", expected
                            , " expected, got ", got])
            (x undefined) l

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newcfunction :: LuaImport a => a -> IO (FunPtr LuaCFunction)
newcfunction = mkWrapper . luaimport

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of StackValue
--   * return type is IO t, where t is an instance of StackValue
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
luaimport :: LuaImport a => a -> LuaCFunction
luaimport a l = luaimport' 1 a l

-- | Free function pointer created with @newcfunction@.
freecfunction :: FunPtr LuaCFunction -> IO ()
freecfunction = freeHaskellFunPtr

class LuaCallProc a where
    callproc' :: LuaState -> String -> IO () -> NumArgs -> a

-- | Call a Lua procedure. Use as:
--
-- > callproc l "proc" "abc" (1::Int) (5.0::Double)
--
callproc :: (LuaCallProc a) => LuaState -> String -> a
callproc l f = callproc' l f (return ()) 0

class LuaCallFunc a where
    callfunc' :: LuaState -> String -> IO () -> NumArgs -> a

-- | Call a Lua function. Use as:
--
-- > Just v <- callfunc l "proc" "abc" (1::Int) (5.0::Double)
callfunc :: (LuaCallFunc a) => LuaState -> String -> a
callfunc l f = callfunc' l f (return ()) 0

instance LuaCallProc (IO t) where
    callproc' l f a k = do
      getglobal2 l f
      a
      z <- pcall l k 0 0
      if z /= 0
        then do
          Just msg <- peek l (-1)
          pop l 1
          fail (BC.unpack msg)
        else return undefined

instance (StackValue t) => LuaCallFunc (IO t) where
    callfunc' l f a k = do
      getglobal2 l f
      a
      z <- pcall l k 1 0
      if z/=0
        then do
          Just msg <- peek l (-1)
          pop l 1
          fail (BC.unpack msg)
        else do
          r <- peek l (-1)
          pop l 1
          case r of
            Just x -> return x
            Nothing -> do
              expected <- typename l (valuetype (fromJust r))
              t <- ltype l (-1)
              got <- typename l t
              fail $ Prelude.concat
                [ "Incorrect result type (", expected, " expected, got ", got, ")" ]

instance (StackValue t, LuaCallProc b) => LuaCallProc (t -> b) where
    callproc' l f a k x = callproc' l f (a >> push l x) (k+1)

instance (StackValue t, LuaCallFunc b) => LuaCallFunc (t -> b) where
    callfunc' l f a k x = callfunc' l f (a >> push l x) (k+1)


foreign export ccall hsmethod__gc :: LuaState -> IO CInt
foreign import ccall "&hsmethod__gc" hsmethod__gc_addr :: FunPtr LuaCFunction

foreign export ccall hsmethod__call :: LuaState -> IO CInt
foreign import ccall "&hsmethod__call" hsmethod__call_addr :: FunPtr LuaCFunction

hsmethod__gc :: LuaState -> IO CInt
hsmethod__gc l = do
    Just ptr <- peek l (-1)
    stableptr <- F.peek (castPtr ptr)
    freeStablePtr stableptr
    return 0

hsmethod__call :: LuaState -> IO CInt
hsmethod__call l = do
    Just ptr <- peek l 1
    remove l 1
    stableptr <- F.peek (castPtr ptr)
    f <- deRefStablePtr stableptr
    f l


-- | Pushes Haskell function converted to a Lua function.
-- All values created will be garbage collected. Use as:
--
-- > Lua.pushhsfunction l myfun
-- > Lua.setglobal l "myfun"
--
-- You are not allowed to use @lua_error@ anywhere, but
-- use an error code of (-1) to the same effect. Push
-- error message as the sole return value.
pushhsfunction :: LuaImport a => LuaState -> a -> IO ()
pushhsfunction l f = pushrawhsfunction l (luaimport f)

-- | Pushes _raw_ Haskell function converted to a Lua function.
-- Raw Haskell functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left in the stack.
pushrawhsfunction :: LuaState -> LuaCFunction -> IO ()
pushrawhsfunction l f = do
    stableptr <- newStablePtr f
    p <- newuserdata l (F.sizeOf stableptr)
    F.poke (castPtr p) stableptr
    v <- newmetatable l "HaskellImportedFunction"
    when (v /= 0) $ do
      -- create new metatable, fill it with two entries __gc and __call
      push l hsmethod__gc_addr
      setfield l (-2) "__gc"
      push l hsmethod__call_addr
      setfield l (-2) "__call"
    setmetatable l (-2)
    return ()

-- | Imports a Haskell function and registers it at global name.
registerhsfunction :: LuaImport a => LuaState -> String -> a -> IO ()
registerhsfunction l n f = pushhsfunction l f >> setglobal l n

-- | Imports a raw Haskell function and registers it at global name.
registerrawhsfunction :: LuaState -> String -> (LuaState -> IO CInt) -> IO ()
registerrawhsfunction l n f = pushrawhsfunction l f >> setglobal l n

-- * Error handling in hslua
-- $ Error handling in hslua is tricky, because we can call Haskell from Lua which calls
-- Lua again etc. (or the other way around, e.g. Lua loads Haskell program
-- compiled as a dynamic library, see [this blog
-- post](http://osa1.net/posts/2015-01-16-haskell-so-lua.html) as an example)
--
-- At each language boundary we should check for errors and propagate them properly
-- to the next level in stack.
--
-- Let's say we have this call stack: (stack grows upwards)
--
-- > Haskell function
-- > Lua function
-- > Haskell program
--
-- and we want to report an error in top-most Haskell function. We can't use
-- `lua_error` from Lua C API, because it uses `longjmp`, which means it skips
-- layers of abstractions, including Haskell RTS. There's no way to prevent this
-- `longjmp`. `lua_pcall` sets the jump target, but even with `lua_pcall` it's
-- not safe. Consider this call stack:
--
-- > Haskell function which calls `lua_error`
-- > Lua function, uses pcall
-- > Haskell program
--
-- This program jumps to Lua function, skipping Haskell RTS code that would run
-- before Haskell function returns. For this reason we can use
-- `lua_pcall`(`pcall`) only for catching errors from Lua, and even in that case
-- we need to make sure there are no Haskell calls between error-throwing Lua
-- call and our `pcall` call.
--
-- To be able to catch errors from Haskell functions in Lua, we need to find a
-- convention. Currently hslua does this: `lerror` has same type as Lua's
-- `lua_error`, but instead of calling real `lua_error`, it's returning two
-- values: A special value `_HASKELLERR` and error message as a string.
--
-- Using this, we can write a function to catch errors from Haskell like this:
--
-- > function catch_haskell(ret, err_msg)
-- >     if ret == _HASKELLERR then
-- >       print("Error caught from Haskell land: " .. err_msg)
-- >       return
-- >     end
-- >     return ret
-- > end
--
-- (`_HASKELLERR` is created by `newstate`)
--
-- (Type errors in Haskell functions are also handled using this convention.
-- E.g.  if you pass a Lua value with wrong type to a Haskell function, error
-- will be reported in this way)
--
-- At this point our call stack is like this:
--
-- > Lua function (Haskell function returned with error, which we caught)
-- > Haskell program
--
-- If we further want to propagate the error message to Haskell program, we
-- we can just use standard @error@ function and use `pcall` in Haskell side.
-- Note that if we use @error@ in Lua side and forget to use `pcall` in calling
-- Haskell function, we start skipping layers of abstractions and we get a
-- segfault in the best case.
--
-- This use of @error@ in Lua side and `pcall` in Haskell side is safe, as
-- long as there are no Haskell-Lua interactions going on between those two
-- calls. (e.g. we can only remove one layer from our stack, otherwise it's
-- unsafe)
--
-- The reason it's safe is because `lua_pcall` C function is calling the Lua
-- function using Lua C API, and when called Lua function calls @error@ it
-- `longjmp`s to `lua_pcall` C function, without skipping any layers of
-- abstraction. `lua_pcall` then returns to Haskell.
--
-- As an example program that does error propagations between Haskell and Lua(in
-- both ways), see [this
-- example](https://github.com/osa1/hslua/tree/master/examples/err_prop) from
-- hslua repository.
--
-- NOTE: If you're loading a hslua program compiled to a dynamic library from a
-- Lua program, you need to define @_HASKELLERR = {}@ manually, after creating
-- the Lua state.

-- * A note about integer functions
-- $ Lua didn't have integers until Lua 5.3, and the version supported by hslua
-- is Lua 5.1. In Lua 5.1 and 5.2, integer functions like 'pushinteger' convert
-- integers to 'LuaNumber's before storing them in Lua stack/heap, and getter
-- functions like 'tointeger' convert them back to 'LuaInteger's.
--
-- This means that you can lose some information during the conversion. For
-- example:
--
-- > main = do
-- >   l <- newstate
-- >   let val = maxBound :: LuaInteger
-- >   pushinteger l val
-- >   i3 <- tointeger l 1
-- >   putStrLn $ show val ++ " - " ++ show i3
--
-- Prints @9223372036854775807 - -9223372036854775808@.
