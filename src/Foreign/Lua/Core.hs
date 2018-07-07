{-
Copyright © 2007-2012 Gracjan Polak
Copyright © 2012-2016 Ömer Sinan Ağacan
Copyright © 2017-2018 Albert Krewinkel

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
{-|
Module      : Foreign.Lua.Core
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : CPP, ForeignFunctionInterface

Monadic functions which operate within the Lua type.

The functions in this module are mostly just thin wrappers around the respective
C functions. However, C function which can throw an error are wrapped such that
the error is converted into a @'LuaException'@. Memory allocation errors,
however, are not caught and will cause the host program to terminate.
-}
module Foreign.Lua.Core (
  -- * Lua Computations
    Lua (..)
  , runLuaWith
  , liftIO
  , luaState
  -- * Lua API types
  , CFunction
  , LuaBool (..)
  , false
  , true
  , fromLuaBool
  , toLuaBool
  , LuaInteger (..)
  , LuaNumber (..)
  , StackIndex (..)
  , nthFromBottom
  , nthFromTop
  , stackTop
  , stackBottom
  , NumArgs (..)
  , NumResults (..)
  -- * Lua API
  -- ** Constants and pseudo-indices
  , multret
  , registryindex
  , noref
  , refnil
  , upvalueindex
  -- ** State manipulation
  , LuaState (..)
  , newstate
  , close
  -- ** Basic stack manipulation
  , absindex
  , gettop
  , settop
  , pushvalue
  , copy
  , insert
  , pop
  , remove
  , replace
  , checkstack
  -- ** types and type checks
  , Type (..)
  , TypeCode (..)
  , fromType
  , toType
  , ltype
  , typename
  , isboolean
  , iscfunction
  , isfunction
  , islightuserdata
  , isnil
  , isnone
  , isnoneornil
  , isnumber
  , isstring
  , istable
  , isthread
  , isuserdata
  -- ** access functions (stack → Haskell)
  , toboolean
  , tocfunction
  , tointeger
  , tointegerx
  , tonumber
  , tonumberx
  , topointer
  , tostring
  , tothread
  , touserdata
  , objlen
  , rawlen
  , strlen
  -- ** Comparison and arithmetic functions
  , RelationalOperator (..)
  , fromRelationalOperator
  , compare
  , equal
  , lessthan
  , rawequal
  -- ** push functions (Haskell → stack)
  , pushboolean
  , pushcfunction
  , pushcclosure
  , pushinteger
  , pushlightuserdata
  , pushnil
  , pushnumber
  , pushstring
  , pushthread
  -- ** get functions (Lua → stack)
  , getglobal
  , gettable
  , getfield
  , rawget
  , rawgeti
  , createtable
  , newtable
  , newuserdata
  , getmetatable
  -- ** set functions (stack → Lua)
  , setglobal
  , settable
  , setfield
  , rawset
  , rawseti
  , setmetatable
  -- ** load and call functions (load and run Lua code)
  , call
  , pcall
  , load
  , loadbuffer
  , loadfile
  , loadstring
  -- ** Coroutine functions
  , Status (..)
  , toStatus
  , status
  -- ** garbage-collection function and options
  , GCCONTROL (..)
  , gc
  -- ** miscellaneous and helper functions
  , next
  , error
  , concat
  , register
  -- * loading libraries
  , openbase
  , opendebug
  , openio
  , openlibs
  , openmath
  , openpackage
  , openos
  , openstring
  , opentable
  -- * Auxiliary library
  , dostring
  , dofile
  , newmetatable
  , tostring'
  , ref
  , unref
  -- * Error handling
  -- | We are trying to keep error handling on the haskell side very simple and
  -- intuitive. However, when combined with error handling on the Lua side, it
  -- get's tricky: We can call Haskell from Lua which calls Lua again etc. At
  -- each language boundary we should check for errors and propagate them
  -- properly to the next level in stack. Hslua does this for you when returning
  -- from Lua to Haskell, but care must be taken when passing errors back into
  -- Lua.
  --
  -- Let's say we have this call stack: (stack grows upwards)
  --
  -- > Haskell function
  -- > Lua function
  -- > Haskell program
  --
  -- and we want to report an error in the top-most Haskell function. We can't
  -- use @lua_error@ from the Lua C API, because it uses @longjmp@, which means
  -- it skips layers of abstractions, including the Haskell RTS. There's no way
  -- to prevent this @longjmp@. @lua_pcall@ sets the jump target, but even with
  -- @lua_pcall@ it's not safe. Consider this call stack:
  --
  -- > Haskell function which calls lua_error
  -- > Lua function, uses pcall
  -- > Haskell program
  --
  -- This program jumps to Lua function, skipping Haskell RTS code that would run
  -- before Haskell function returns. For this reason we can use
  -- @lua_pcall@ (@'pcall'@) only for catching errors from Lua, and even in that case
  -- we need to make sure there are no Haskell calls between error-throwing Lua
  -- call and our @'pcall'@ call.
  --
  -- To be able to catch errors from Haskell functions in Lua, we need to find a
  -- convention. Currently hslua does this: @'error'@ has same type as Lua's
  -- @lua_error@, but instead of calling real @lua_error@, it's returning two
  -- values: A special error value and an error message as a string.
  --
  -- These internals should stay hidden most of the time, as all
  -- Haskell functions are wrapped in such a way, that error values are
  -- transformed into Lua errors behind the scenes.
  --
  -- At this point our call stack is like this:
  --
  -- > Lua function (Haskell function returned with error, which we caught)
  -- > Haskell program
  --
  -- If we further want to propagate the error message to Haskell program, we we
  -- can just use standard @error@ function and use @'pcall'@ in Haskell side.
  -- Note that if we use @error@ on the Lua side and forget to use `pcall` in
  -- the calling Haskell function, we would be starting to skip layers of
  -- abstractions and would get a segfault in the best case. That's why hslua
  -- wraps all API functions that can potentially fail in custom C functions.
  -- Those functions behave idential to the functions they wrap, but catch all
  -- errors and return error codes instead. Using @error@ within Lua should
  -- hence be safe.
  --
  -- However, the raw C API bindings in @'Foreign.Lua.Core.RawBindings'@ don't
  -- provide these guarantees. Even an apparently harmless operations like
  -- accessing a field via @'lua_getfield'@ can call a meta method and trigger a
  -- @longjmp@, causing the host program to crash.
  --
  -- The @'pcall'@ function is not wrapped in additional C code but still safe.
  -- The reason it's safe is because the @lua_pcall@ C function is calling the
  -- Lua function using Lua C API, and when the called Lua function calls
  -- @error@ it @longjmp@s to @lua_pcall@ C function, without skipping any
  -- layers of abstraction. @lua_pcall@ then returns to Haskell.
  --
  -- NOTE: If you're loading a hslua program compiled to a dynamic library from
  -- a Lua program, you need to set @HSLUA_ERR@ in the registry to any unique
  -- value manually, after creating the Lua state.
  , LuaException (..)
  , catchLuaError
  , throwLuaError
  , modifyLuaError
  , tryLua
  , throwTopMessageAsError
  ) where

import Prelude hiding (EQ, LT, compare, concat, error)

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Foreign.C
import Foreign.Lua.Core.Constants
import Foreign.Lua.Core.Error
import Foreign.Lua.Core.RawBindings
import Foreign.Lua.Core.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.Storable as F

--
-- Helper functions
--

-- | Convert the object at the top of the stack into a string and throw it as a
-- @'LuaException'@.
throwTopMessageAsError :: Lua a
throwTopMessageAsError = do
  msg <- tostring' stackTop
  pop 2 -- remove error and error string pushed by tostring'
  throwLuaError msg


-- | Convert from Failable to target type, throwing an error if the value
-- indicates a failure.
fromFailable :: (CInt -> a) -> Failable a -> Lua a
fromFailable fromCInt (Failable x) =
  if x < 0
  then throwTopMessageAsError
  else return (fromCInt x)

-- | Throw a lua error if the computation signaled a failure.
throwOnError :: Failable () -> Lua ()
throwOnError = fromFailable (const ())

-- | Convert lua boolean to Haskell Bool, throwing an exception if the return
-- value indicates that an error had happened.
boolFromFailable :: Failable LuaBool -> Lua Bool
boolFromFailable = fmap fromLuaBool . fromFailable LuaBool

--
-- API functions
--

-- | Converts the acceptable index @idx@ into an equivalent absolute index (that
-- is, one that does not depend on the stack top).
absindex :: StackIndex -> Lua StackIndex
absindex = liftLua1 lua_absindex

-- |  Calls a function.
--
-- To call a function you must use the following protocol: first, the function
-- to be called is pushed onto the stack; then, the arguments to the function
-- are pushed in direct order; that is, the first argument is pushed first.
-- Finally you call @call@; @nargs@ is the number of arguments that you pushed
-- onto the stack. All arguments and the function value are popped from the
-- stack when the function is called. The function results are pushed onto the
-- stack when the function returns. The number of results is adjusted to
-- @nresults@, unless @nresults@ is @multret@. In this case, all results from
-- the function are pushed. Lua takes care that the returned values fit into the
-- stack space. The function results are pushed onto the stack in direct order
-- (the first result is pushed first), so that after the call the last result is
-- on the top of the stack.
--
-- Any error inside the called function cause a @'LuaException'@ to be thrown.
--
-- The following example shows how the host program can do the equivalent to
-- this Lua code:
--
-- > a = f("how", t.x, 14)
--
-- Here it is in Haskell (assuming the OverloadedStrings language extension):
--
-- > getglobal "f"         -- function to be called
-- > pushstring  "how"     -- 1st argument
-- > getglobal "t"         -- table to be indexed
-- > getfield (-1) "x"     -- push result of t.x (2nd arg)
-- > remove (-2)           -- remove 't' from the stack
-- > pushinteger 14        -- 3rd argument
-- > call 3 1              -- call 'f' with 3 arguments and 1 result
-- > setglobal "a"         -- set global 'a'
--
-- Note that the code above is "balanced": at its end, the stack is back to its
-- original configuration. This is considered good programming practice.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_call lua_call>.
call :: NumArgs -> NumResults -> Lua ()
call nargs nresults = do
  res <- pcall nargs nresults Nothing
  when (res /= OK) throwTopMessageAsError

-- | Ensures that the stack has space for at least @n@ extra slots (that is,
-- that you can safely push up to @n@ values into it). It returns false if it
-- cannot fulfill the request, either because it would cause the stack to be
-- larger than a fixed maximum size (typically at least several thousand
-- elements) or because it cannot allocate memory for the extra space. This
-- function never shrinks the stack; if the stack already has space for the
-- extra slots, it is left unchanged.
--
-- This is a wrapper function of
-- <https://www.lua.org/manual/5.3/manual.html#lua_checkstack lua_checkstack>.
checkstack :: Int -> Lua Bool
checkstack n = liftLua $ \l -> fromLuaBool <$> lua_checkstack l (fromIntegral n)

-- | Destroys all objects in the given Lua state (calling the corresponding
-- garbage-collection metamethods, if any) and frees all dynamic memory used by
-- this state. On several platforms, you may not need to call this function,
-- because all resources are naturally released when the host program ends. On
-- the other hand, long-running programs that create multiple states, such as
-- daemons or web servers, will probably need to close states as soon as they
-- are not needed.
--
-- This is a wrapper function of
-- <https://www.lua.org/manual/5.3/manual.html#lua_close lua_close>.
close :: LuaState -> IO ()
close = lua_close

-- | Compares two Lua values. Returns @True@ if the value at index @idx1@
-- satisfies @op@ when compared with the value at index @idx2@, following the
-- semantics of the corresponding Lua operator (that is, it may call
-- metamethods). Otherwise returns @False@. Also returns @False@ if any of the
-- indices is not valid.
--
-- The value of op must be of type @'LuaComparerOp'@:
--
--    OpEQ: compares for equality (==)
--    OpLT: compares for less than (<)
--    OpLE: compares for less or equal (<=)
--
-- This is a wrapper function of
-- <https://www.lua.org/manual/5.3/manual.html#lua_compare lua_compare>.
compare :: StackIndex -> StackIndex -> RelationalOperator -> Lua Bool
compare idx1 idx2 relOp = boolFromFailable =<< do
  liftLua $ \l -> hslua_compare l idx1 idx2 (fromRelationalOperator relOp)

-- | Concatenates the @n@ values at the top of the stack, pops them, and leaves
-- the result at the top. If @n@ is 1, the result is the single value on the
-- stack (that is, the function does nothing); if @n@ is 0, the result is the
-- empty string. Concatenation is performed following the usual semantics of Lua
-- (see <https://www.lua.org/manual/5.3/manual.html#3.4.6 §3.4.6> of the lua
-- manual).
--
-- This is a wrapper function of
-- <https://www.lua.org/manual/5.3/manual.html#lua_concat lua_concat>.
concat :: NumArgs -> Lua ()
concat n = throwOnError =<< liftLua (`hslua_concat` n)

-- | Copies the element at index @fromidx@ into the valid index @toidx@,
-- replacing the value at that position. Values at other positions are not
-- affected.
--
-- See also <https://www.lua.org/manual/5.3/manual.html#lua_copy lua_copy> in
-- the lua manual.
copy :: StackIndex -> StackIndex -> Lua ()
copy fromidx toidx = liftLua $ \l -> lua_copy l fromidx toidx

-- | Creates a new empty table and pushes it onto the stack. Parameter narr is a
-- hint for how many elements the table will have as a sequence; parameter nrec
-- is a hint for how many other elements the table will have. Lua may use these
-- hints to preallocate memory for the new table. This preallocation is useful
-- for performance when you know in advance how many elements the table will
-- have. Otherwise you can use the function lua_newtable.
--
-- This is a wrapper for function
-- <https://www.lua.org/manual/5.3/manual.html#lua_createtable lua_createtable>.
createtable :: Int -> Int -> Lua ()
createtable narr nrec = liftLua $ \l ->
  lua_createtable l (fromIntegral narr) (fromIntegral nrec)

-- | Loads and runs the given string.
--
-- Returns @'OK'@ on success, or an error if either loading of the string or
-- calling of the thunk failed.
dostring :: String -> Lua Status
dostring s = do
  loadRes <- loadstring s
  if loadRes == OK
    then pcall 0 multret Nothing
    else return loadRes

-- | Loads and runs the given file.
dofile :: FilePath -> Lua Status
dofile fp = do
  loadRes <- loadfile fp
  if loadRes == OK
    then pcall 0 multret Nothing
    else return loadRes

-- TODO: implement dump

-- | Returns @True@ if the two values in acceptable indices index1 and index2
-- are equal, following the semantics of the Lua @==@ operator (that is, may
-- call metamethods). Otherwise returns False. Also returns False if any of the
-- indices is non valid. Uses @'compare'@ internally.
equal :: StackIndex -> StackIndex -> Lua Bool
equal index1 index2 = compare index1 index2 EQ

-- | This is a convenience function to implement error propagation convention
-- described in [Error handling in hslua](#g:1). hslua doesn't implement
-- `lua_error` function from Lua C API because it's never safe to use. (see
-- [Error handling in hslua](#g:1) for details)
error :: Lua NumResults
error = do
  getfield registryindex "HSLUA_ERR"
  insert (-2)
  return 2

-- |  Controls the garbage collector.
--
-- This function performs several tasks, according to the value of the parameter
-- what:
--
--   * @'GCSTOP'@: stops the garbage collector.
--
--   * @'GCRESTART'@: restarts the garbage collector.
--
--   * @'GCCOLLECT'@: performs a full garbage-collection cycle.
--
--   * @'GCCOUNT'@: returns the current amount of memory (in Kbytes) in use by
--     Lua.
--
--   * @'GCCOUNTB'@: returns the remainder of dividing the current amount of
--     bytes of memory in use by Lua by 1024.
--
--   * @'GCSTEP'@: performs an incremental step of garbage collection. The step
--     "size" is controlled by data (larger values mean more steps) in a
--     non-specified way. If you want to control the step size you must
--     experimentally tune the value of data. The function returns 1 if the step
--     finished a garbage-collection cycle.
--
--   * @'GCSETPAUSE@': sets data as the new value for the pause of the collector
--     (see §2.10). The function returns the previous value of the pause.
--
--   * @'GCSETSTEPMUL'@: sets data as the new value for the step multiplier of
--     the collector (see §2.10). The function returns the previous value of the
--     step multiplier.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_gc lua_gc>.
gc :: GCCONTROL -> Int -> Lua Int
gc what data' = liftLua $ \l ->
  fromIntegral <$> lua_gc l (fromIntegral (fromEnum what)) (fromIntegral data')

-- | Pushes onto the stack the value @t[k]@, where @t@ is the value at the given
-- stack index. As in Lua, this function may trigger a metamethod for the
-- "index" event (see <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of
-- lua's manual).
--
-- Errors on the Lua side are caught and rethrown as LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_getfield lua_getfield>.
getfield :: StackIndex -> String -> Lua ()
getfield i s = throwOnError =<< liftLua
  (\l -> withCString s (hslua_getfield l i))

-- | Pushes onto the stack the value of the global @name@.
--
-- Errors on the Lua side are caught and rethrown as LuaException.
--
-- Wrapper of
-- <https://www.lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>.
getglobal :: String -> Lua ()
getglobal name = throwOnError =<<
  liftLua (withCString name . hslua_getglobal)

-- | If the value at the given index has a metatable, the function pushes that
-- metatable onto the stack and returns @True@. Otherwise, the function returns
-- @False@ and pushes nothing on the stack.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_getmetatable lua_getmetatable>.
getmetatable :: StackIndex -> Lua Bool
getmetatable n = liftLua $ \l ->
  fromLuaBool <$> lua_getmetatable l n

-- | Pushes onto the stack the value @t[k]@, where @t@ is the value at the given
-- index and @k@ is the value at the top of the stack.
--
-- This function pops the key from the stack, pushing the resulting value in its
-- place. As in Lua, this function may trigger a metamethod for the "index"
-- event (see <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of lua's
-- manual).
--
-- Errors on the Lua side are caught and rethrown as LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>.
gettable :: StackIndex -> Lua ()
gettable n = throwOnError =<<
  liftLua (\l -> hslua_gettable l n)

-- | Returns the index of the top element in the stack. Because indices start at
-- 1, this result is equal to the number of elements in the stack (and so 0
-- means an empty stack).
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#lua_gettop lua_gettop>.
gettop :: Lua StackIndex
gettop = liftLua lua_gettop

-- | Moves the top element into the given valid index, shifting up the elements
-- above this index to open space. This function cannot be called with a
-- pseudo-index, because a pseudo-index is not an actual stack position.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_insert lua_insert>.
insert :: StackIndex -> Lua ()
insert index = liftLua $ \l -> lua_insert l index

-- | Returns @True@ if the value at the given index is a boolean, and @False@
-- otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isboolean lua_isboolean>.
isboolean :: StackIndex -> Lua Bool
isboolean n = (== TypeBoolean) <$> ltype n

-- | Returns @True@ if the value at the given index is a C function, and @False@
-- otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_iscfunction lua_iscfunction>.
iscfunction :: StackIndex -> Lua Bool
iscfunction n = liftLua $ \l -> fromLuaBool <$> lua_iscfunction l n

-- | Returns @True@ if the value at the given index is a function (either C or
-- Lua), and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isfunction lua_isfunction>.
isfunction :: StackIndex -> Lua Bool
isfunction n = (== TypeFunction) <$> ltype n

-- | Returns @True@ if the value at the given index is a light userdata, and
-- @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_islightuserdata \
-- lua_islightuserdata>.
islightuserdata :: StackIndex -> Lua Bool
islightuserdata n = (== TypeLightUserdata) <$> ltype n

-- | Returns @True@ if the value at the given index is @nil@, and @False@
-- otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isnil lua_isnil>.
isnil :: StackIndex -> Lua Bool
isnil n = (== TypeNil) <$> ltype n

-- | Returns @True@ if the given index is not valid, and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isnone lua_isnone>.
isnone :: StackIndex -> Lua Bool
isnone n = (== TypeNone) <$> ltype n

-- | Returns @True@ if the given index is not valid or if the value at the given
-- index is @nil@, and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isnoneornil lua_isnoneornil>.
isnoneornil :: StackIndex -> Lua Bool
isnoneornil idx = (<= TypeNil) <$> ltype idx

-- | Returns @True@ if the value at the given index is a number or a string
-- convertible to a number, and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isnumber lua_isnumber>.
isnumber :: StackIndex -> Lua Bool
isnumber n = liftLua $ \l -> fromLuaBool <$> lua_isnumber l n

-- | Returns @True@ if the value at the given index is a string or a number
-- (which is always convertible to a string), and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isstring lua_isstring>.
isstring :: StackIndex -> Lua Bool
isstring n = liftLua $ \l -> fromLuaBool <$> lua_isstring l n

-- | Returns @True@ if the value at the given index is a table, and @False@
-- otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_istable lua_istable>.
istable :: StackIndex -> Lua Bool
istable n = (== TypeTable) <$> ltype n

-- | Returns @True@ if the value at the given index is a thread, and @False@
-- otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isthread lua_isthread>.
isthread :: StackIndex -> Lua Bool
isthread n = (== TypeThread) <$> ltype n

-- | Returns @True@ if the value at the given index is a userdata (either full
-- or light), and @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_isuserdata lua_isuserdata>.
isuserdata :: StackIndex -> Lua Bool
isuserdata n = liftLua $ \l -> fromLuaBool <$> lua_isuserdata l n

-- | Tests whether the object under the first index is smaller than that under
-- the second. Uses @'compare'@ internally.
lessthan :: StackIndex -> StackIndex -> Lua Bool
lessthan index1 index2 = compare index1 index2 LT

-- | Loads a Lua chunk (without running it). If there are no errors, @'load'@
-- pushes the compiled chunk as a Lua function on top of the stack. Otherwise,
-- it pushes an error message.
--
-- The return values of @'load'@ are:
--
-- - @'OK'@: no errors;
-- - @'ErrSyntax'@: syntax error during pre-compilation;
-- - @'ErrMem'@: memory allocation error;
-- - @'ErrGcmm'@: error while running a @__gc@ metamethod. (This error has no
--   relation with the chunk being loaded. It is generated by the garbage
--   collector.)
--
-- This function only loads a chunk; it does not run it.
--
-- @load@ automatically detects whether the chunk is text or binary, and loads
-- it accordingly (see program luac).
--
-- The @'load'@ function uses a user-supplied reader function to read the chunk
-- (see @'LuaReader'@). The data argument is an opaque value passed to the
-- reader function.
--
-- The chunkname argument gives a name to the chunk, which is used for error
-- messages and in debug information (see
-- <https://www.lua.org/manual/5.1/manual.html#3.8 §3.8>).
load :: LuaReader -> Ptr () -> String -> Lua Status
load reader data' name = liftLua $ \l ->
  withCString name $ \namePtr ->
  toStatus <$> lua_load l reader data' namePtr nullPtr

-- | Loads a ByteString as a Lua chunk.
--
-- This function returns the same results as @'load'@. @name@ is the chunk name,
-- used for debug information and error messages.
--
-- See <https://www.lua.org/manual/5.3/manual.html#luaL_loadbuffer luaL_loadbuffer>.
loadbuffer :: ByteString -> String -> Lua Status
loadbuffer bs name = liftLua $ \l ->
  B.useAsCStringLen bs $ \(str, len) ->
  withCString name $ \namePtr ->
  toStatus <$> luaL_loadbuffer l str (fromIntegral len) namePtr

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: String -> Lua Status
loadfile f = liftLua $ \l ->
  withCString f $ \fPtr ->
  toStatus <$> luaL_loadfile l fPtr

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: String -> Lua Status
loadstring str = loadbuffer (Char8.pack str) (filter (/= '\NUL') str)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>.
ltype :: StackIndex -> Lua Type
ltype idx = toType <$> liftLua (flip lua_type idx)

-- | If the registry already has the key tname, returns @False@. Otherwise,
-- creates a new table to be used as a metatable for userdata, adds to this new
-- table the pair @__name = tname@, adds to the registry the pair @[tname] = new
-- table@, and returns @True@. (The entry @__name@ is used by some
-- error-reporting functions.)
--
-- In both cases pushes onto the stack the final value associated with @tname@ in
-- the registry.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: String -> Lua Bool
newmetatable tname = liftLua $ \l ->
  fromLuaBool <$> withCString tname (luaL_newmetatable l)

-- | Creates a new Lua state. It calls @'lua_newstate'@ with an allocator based
-- on the standard C @realloc@ function and then sets a panic function (see
-- <https://www.lua.org/manual/5.3/manual.html#4.6 §4.6> of the Lua 5.3
-- Reference Manual) that prints an error message to the standard error output
-- in case of fatal errors.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO LuaState
newstate = do
  l <- luaL_newstate
  runLuaWith l $ do
    createtable 0 0
    setfield registryindex "HSLUA_ERR"
    return l

-- | Creates a new empty table and pushes it onto the stack. It is equivalent to
-- @createtable 0 0@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_newtable lua_newtable>.
newtable :: Lua ()
newtable = createtable 0 0

-- | This function allocates a new block of memory with the given size, pushes
-- onto the stack a new full userdata with the block address, and returns this
-- address. The host program can freely use this memory.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_newuserdata lua_newuserdata>.
newuserdata :: Int -> Lua (Ptr ())
newuserdata = liftLua1 lua_newuserdata . fromIntegral

-- | Pops a key from the stack, and pushes a key–value pair from the table at
-- the given index (the "next" pair after the given key). If there are no more
-- elements in the table, then @next@ returns @False@ (and pushes nothing).
--
-- Errors on the Lua side are caught and rethrown as a LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_next lua_next>.
next :: StackIndex -> Lua Bool
next idx = boolFromFailable =<< liftLua (\l -> hslua_next l idx)

{-# DEPRECATED objlen "Use rawlen instead." #-}
-- | Obsolete alias for @'rawlen'@.
objlen :: StackIndex -> Lua Int
objlen = rawlen

-- | Opens all standard Lua libraries into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_openlibs luaL_openlibs>.
openlibs :: Lua ()
openlibs = liftLua luaL_openlibs

-- | Opens all standard Lua libraries into the current state.
--
-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_base luaopen_base>.
openbase :: Lua ()
openbase = pushcfunction lua_open_base_ptr *> call 0 multret

-- | Opens Lua's /debug/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_debug luaopen_debug>.
opendebug :: Lua ()
opendebug = pushcfunction lua_open_debug_ptr *> call 0 multret

-- | Opens Lua's /io/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_io luaopen_io>.
openio :: Lua ()
openio = pushcfunction lua_open_io_ptr *> call 0 multret

-- | Opens Lua's /math/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_math luaopen_math>.
openmath :: Lua ()
openmath = pushcfunction lua_open_math_ptr *> call 0 multret

-- | Opens Lua's /os/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_os luaopen_os>.
openos :: Lua ()
openos = pushcfunction lua_open_os_ptr *> call 0 multret

-- | Opens Lua's /package/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_package luaopen_package>.
openpackage :: Lua ()
openpackage = pushcfunction lua_open_package_ptr *> call 0 multret

-- | Opens Lua's /string/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_string luaopen_string>.
openstring :: Lua ()
openstring = pushcfunction lua_open_string_ptr *> call 0 multret

-- | Opens Lua's /table/ library into the current state.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_table luaopen_table>.
opentable :: Lua ()
opentable = pushcfunction lua_open_table_ptr *> call 0 multret

-- | Calls a function in protected mode.
--
-- Both @nargs@ and @nresults@ have the same meaning as in @'call'@. If there
-- are no errors during the call, @pcall@ behaves exactly like @'call'@.
-- However, if there is any error, @pcall@ catches it, pushes a single value on
-- the stack (the error message), and returns the error code. Like @'call'@,
-- @pcall@ always removes the function and its arguments from the stack.
--
-- If @msgh@ is @Nothing@, then the error object returned on the stack is
-- exactly the original error object. Otherwise, when @msgh@ is @Just idx@, the
-- stack index @idx@ is the location of a message handler. (This index cannot be
-- a pseudo-index.) In case of runtime errors, this function will be called with
-- the error object and its return value will be the object returned on the
-- stack by @'pcall'@.
--
-- Typically, the message handler is used to add more debug information to the
-- error object, such as a stack traceback. Such information cannot be gathered
-- after the return of @'pcall'@, since by then the stack has unwound.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_pcall lua_pcall>.
pcall :: NumArgs -> NumResults -> Maybe StackIndex -> Lua Status
pcall nargs nresults msgh = liftLua $ \l ->
  toStatus <$> lua_pcall l nargs nresults (fromMaybe 0 msgh)

-- | Pops @n@ elements from the stack.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#lua_pop lua_pop>.
pop :: StackIndex -> Lua ()
pop n = settop (-n - 1)

-- | Pushes a boolean value with the given value onto the stack.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushboolean lua_pushboolean>.
pushboolean :: Bool -> Lua ()
pushboolean b = liftLua $ \l -> lua_pushboolean l (toLuaBool b)

-- | Pushes a new C closure onto the stack.
--
-- When a C function is created, it is possible to associate some values with
-- it, thus creating a C closure (see
-- <https://www.lua.org/manual/5.1/manual.html#3.4 §3.4>); these values are then
-- accessible to the function whenever it is called. To associate values with a
-- C function, first these values should be pushed onto the stack (when there
-- are multiple values, the first value is pushed first). Then lua_pushcclosure
-- is called to create and push the C function onto the stack, with the argument
-- @n@ telling how many values should be associated with the function.
-- lua_pushcclosure also pops these values from the stack.
--
-- The maximum value for @n@ is 255.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushcclosure lua_pushcclosure>.
pushcclosure :: CFunction -> NumArgs -> Lua ()
pushcclosure f n = liftLua $ \l -> lua_pushcclosure l f n

-- | Pushes a C function onto the stack. This function receives a pointer to a C
-- function and pushes onto the stack a Lua value of type function that, when
-- called, invokes the corresponding C function.
--
-- Any function to be callable by Lua must follow the correct protocol to
-- receive its parameters and return its results (see @'CFunction'@)
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: CFunction -> Lua ()
pushcfunction f = pushcclosure f 0

-- | Pushes an integer with with the given value onto the stack.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushinteger lua_pushinteger>.
pushinteger :: LuaInteger -> Lua ()
pushinteger = liftLua1 lua_pushinteger

-- |  Pushes a light userdata onto the stack.
--
-- Userdata represent C values in Lua. A light userdata represents a pointer, a
-- @Ptr ()@ (i.e., @void*@ in C lingo). It is a value (like a number): you do
-- not create it, it has no individual metatable, and it is not collected (as it
-- was never created). A light userdata is equal to "any" light userdata with
-- the same C address.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushlightuserdata lua_pushlightuserdata>.
pushlightuserdata :: Ptr a -> Lua ()
pushlightuserdata = liftLua1 lua_pushlightuserdata

-- | Pushes a nil value onto the stack.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_pushnil lua_pushnil>.
pushnil :: Lua ()
pushnil = liftLua lua_pushnil

-- | Pushes a float with the given value onto the stack.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_pushnumber lua_pushnumber>.
pushnumber :: LuaNumber -> Lua ()
pushnumber = liftLua1 lua_pushnumber

-- | Pushes the zero-terminated string pointed to by s onto the stack. Lua makes
-- (or reuses) an internal copy of the given string, so the memory at s can be
-- freed or reused immediately after the function returns.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#lua_pushstring \
-- lua_pushstring>.
pushstring :: ByteString -> Lua ()
pushstring s = liftLua $ \l ->
  B.unsafeUseAsCStringLen s $ \(sPtr, z) -> lua_pushlstring l sPtr (fromIntegral z)

-- | Pushes the current thread onto the stack. Returns @True@ if this thread is
-- the main thread of its state, @False@ otherwise.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushthread lua_pushthread>.
pushthread :: Lua Bool
pushthread = (1 ==)  <$> liftLua lua_pushthread

-- | Pushes a copy of the element at the given index onto the stack.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_pushvalue lua_pushvalue>.
pushvalue :: StackIndex -> Lua ()
pushvalue n = liftLua $ \l -> lua_pushvalue l n

-- | Returns @True@ if the two values in indices @idx1@ and @idx2@ are
-- primitively equal (that is, without calling the @__eq@ metamethod). Otherwise
-- returns @False@. Also returns @False@ if any of the indices are not valid.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawequal lua_rawequal>.
rawequal :: StackIndex -> StackIndex -> Lua Bool
rawequal idx1 idx2 = liftLua $ \l ->
  fromLuaBool <$> lua_rawequal l idx1 idx2

-- | Similar to @'gettable'@, but does a raw access (i.e., without metamethods).
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawget lua_rawget>.
rawget :: StackIndex -> Lua ()
rawget n = liftLua $ \l -> lua_rawget l n

-- | Pushes onto the stack the value @t[n]@, where @t@ is the table at the given
-- index. The access is raw, that is, it does not invoke the @__index@
-- metamethod.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: StackIndex -> LuaInteger -> Lua ()
rawgeti k m = liftLua $ \l -> lua_rawgeti l k m

-- | Returns the raw "length" of the value at the given index: for strings, this
-- is the string length; for tables, this is the result of the length operator
-- ('#') with no metamethods; for userdata, this is the size of the block of
-- memory allocated for the userdata; for other values, it is 0.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawlen lua_rawlen>.
rawlen :: StackIndex -> Lua Int
rawlen idx = liftLua $ \l -> fromIntegral <$> lua_rawlen l idx

-- | Similar to @'settable'@, but does a raw assignment (i.e., without
-- metamethods).
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawset lua_rawset>.
rawset :: StackIndex -> Lua ()
rawset n = liftLua $ \l -> lua_rawset l n

-- | Does the equivalent of @t[i] = v@, where @t@ is the table at the given
-- index and @v@ is the value at the top of the stack.
--
-- This function pops the value from the stack. The assignment is raw, that is,
-- it does not invoke the @__newindex@ metamethod.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawseti lua_rawseti>.
rawseti :: StackIndex -> LuaInteger -> Lua ()
rawseti k m = liftLua $ \l -> lua_rawseti l k m

-- | Creates and returns a reference, in the table at index @t@, for the object
-- at the top of the stack (and pops the object).
--
-- A reference is a unique integer key. As long as you do not manually add
-- integer keys into table @t@, @ref@ ensures the uniqueness of the key it
-- returns. You can retrieve an object referred by reference @r@ by calling
-- @rawgeti t r@. Function @'unref'@ frees a reference and its associated
-- object.
--
-- If the object at the top of the stack is nil, @'ref'@ returns the constant
-- @'refnil'@. The constant @'noref'@ is guaranteed to be different from any
-- reference returned by @'ref'@.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#luaL_ref luaL_ref>.
ref :: StackIndex -> Lua Int
ref t = liftLua $ \l -> fromIntegral <$> luaL_ref l t

-- | Sets the C function @f@ as the new value of global @name@.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_register lua_register>.
register :: String -> CFunction -> Lua ()
register name f = do
    pushcfunction f
    setglobal name

-- | Removes the element at the given valid index, shifting down the elements
-- above this index to fill the gap. This function cannot be called with a
-- pseudo-index, because a pseudo-index is not an actual stack position.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_remove lua_remove>.
remove :: StackIndex -> Lua ()
remove n = liftLua $ \l -> lua_remove l n

-- | Moves the top element into the given valid index without shifting any
-- element (therefore replacing the value at that given index), and then pops
-- the top element.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_replace lua_replace>.
replace :: StackIndex -> Lua ()
replace n = liftLua $ \l ->  lua_replace l n

-- | Does the equivalent to @t[k] = v@, where @t@ is the value at the given
-- index and @v@ is the value at the top of the stack.
--
-- This function pops the value from the stack. As in Lua, this function may
-- trigger a metamethod for the "newindex" event (see
-- <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of the Lua 5.3
-- Reference Manual).
--
-- Errors on the Lua side are caught and rethrown as a LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setfield lua_setfield>.
setfield :: StackIndex -> String -> Lua ()
setfield i s = throwOnError =<<
  liftLua (\l -> withCString s (hslua_setfield l i))

-- | Pops a value from the stack and sets it as the new value of global @name@.
--
-- Errors on the Lua side are caught and rethrown as a LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setglobal lua_setglobal>.
setglobal :: String -> Lua ()
setglobal s = throwOnError =<<
  liftLua (withCString s . hslua_setglobal)

-- | Pops a table from the stack and sets it as the new metatable for the value
-- at the given index.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setmetatable \
-- lua_setmetatable>.
setmetatable :: StackIndex -> Lua ()
setmetatable idx = liftLua $ \l -> lua_setmetatable l idx

-- | Does the equivalent to @t[k] = v@, where @t@ is the value at the given
-- index, @v@ is the value at the top of the stack, and @k@ is the value just
-- below the top.
--
-- This function pops both the key and the value from the stack. As in Lua, this
-- function may trigger a metamethod for the "newindex" event (see
-- <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of the Lua 5.3
-- Reference Manual).
--
-- Errors on the Lua side are caught and rethrown as a LuaException.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_settable lua_settable>.
settable :: StackIndex -> Lua ()
settable index = throwOnError =<<
  liftLua (\l -> hslua_settable l index)

-- | Accepts any index, or 0, and sets the stack top to this index. If the new
-- top is larger than the old one, then the new elements are filled with nil. If
-- index is 0, then all stack elements are removed.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_settop lua_settop>.
settop :: StackIndex -> Lua ()
settop = liftLua1 lua_settop

-- |  Returns the status of this Lua thread.
--
-- The status can be @'OK'@ for a normal thread, an error value if the thread
-- finished the execution of a @'lua_resume'@ with an error, or @'Yield'@ if
-- the thread is suspended.
--
-- You can only call functions in threads with status @'OK'@. You can resume
-- threads with status @'OK'@ (to start a new coroutine) or @'Yield'@ (to
-- resume a coroutine).
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#lua_status lua_status>.
status :: Lua Status
status = liftLua $ fmap toStatus . lua_status

{-# DEPRECATED strlen "Use rawlen instead." #-}
-- | Compatibility alias for rawlen
strlen :: StackIndex -> Lua Int
strlen = rawlen

-- | Converts the Lua value at the given index to a haskell boolean value. Like
-- all tests in Lua, @toboolean@ returns @True@ for any Lua value different from
-- @false@ and @nil@; otherwise it returns @False@. (If you want to accept only
-- actual boolean values, use @'isboolean'@ to test the value's type.)
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_toboolean lua_toboolean>.
toboolean :: StackIndex -> Lua Bool
toboolean n = liftLua $ \l -> fromLuaBool <$> lua_toboolean l n

-- | Converts a value at the given index to a C function. That value must be a C
-- function; otherwise, returns @nullPtr@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: StackIndex -> Lua CFunction
tocfunction n = liftLua $ \l -> lua_tocfunction l n

-- | Converts the Lua value at the given acceptable index to the signed integral
-- type @'lua_Integer'@. The Lua value must be an integer, a number or a string
-- convertible to an integer (see
-- <https://www.lua.org/manual/5.3/manual.html#3.4.3 §3.4.3> of the Lua 5.3
-- Reference Manual); otherwise, @tointeger@ returns 0.
--
-- If the number is not an integer, it is truncated in some non-specified way.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tointeger lua_tointeger>.
tointeger :: StackIndex -> Lua LuaInteger
tointeger n = liftLua $ \l -> lua_tointeger l n

-- | Like @'tointeger'@, but returns @Nothing@ if the conversion failed
tointegerx :: StackIndex -> Lua (Maybe LuaInteger)
tointegerx n = liftLua $ \l -> alloca $ \bptr -> do
  res <- lua_tointegerx l n bptr
  isNum <- fromLuaBool <$> F.peek bptr
  if isNum
    then return $ Just res
    else return $ Nothing

-- | Converts the Lua value at the given index to the C type lua_Number. The Lua
-- value must be a number or a string convertible to a number; otherwise,
-- @tonumber@ returns 0.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_tonumber lua_tonumber>.
tonumber :: StackIndex -> Lua LuaNumber
tonumber n = liftLua $ \l -> lua_tonumber l n

-- | Like @'tonumber'@, but returns @Nothing@ if the conversion failed
tonumberx :: StackIndex -> Lua (Maybe LuaNumber)
tonumberx n = liftLua $ \l -> alloca $ \bptr -> do
  res <- lua_tonumberx l n bptr
  isNum <- fromLuaBool <$> F.peek bptr
  if isNum
    then return $ Just res
    else return $ Nothing

-- | Converts the value at the given index to a generic C pointer (void*). The
-- value can be a userdata, a table, a thread, or a function; otherwise,
-- lua_topointer returns @nullPtr@. Different objects will give different
-- pointers. There is no way to convert the pointer back to its original value.
--
-- Typically this function is used only for hashing and debug information.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_topointer lua_topointer>.
topointer :: StackIndex -> Lua (Ptr ())
topointer n = liftLua $ \l -> lua_topointer l n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tostring lua_tostring>.
tostring :: StackIndex -> Lua ByteString
tostring n = liftLua $ \l -> alloca $ \lenPtr -> do
  cstr <- lua_tolstring l n lenPtr
  cstrLen <- F.peek lenPtr
  B.packCStringLen (cstr, fromIntegral cstrLen)

-- | Converts any Lua value at the given index to a @'ByteString'@ in a
-- reasonable format. The resulting string is pushed onto the stack and also
-- returned by the function.
--
-- If the value has a metatable with a @__tostring@ field, then @tolstring'@
-- calls the corresponding metamethod with the value as argument, and uses the
-- result of the call as its result.
tostring' :: StackIndex -> Lua B.ByteString
tostring' n = liftLua $ \l -> alloca $ \lenPtr -> do
  cstr <- hsluaL_tolstring l n lenPtr
  if cstr == nullPtr
    then runLuaWith l throwTopMessageAsError
    else do
      cstrLen <- F.peek lenPtr
      B.packCStringLen (cstr, fromIntegral cstrLen)

-- | Converts the value at the given index to a Lua thread (represented as
-- lua_State*). This value must be a thread; otherwise, the function returns
-- @LuaState nullPtr@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tothread lua_tothread>.
tothread :: StackIndex -> Lua LuaState
tothread n = liftLua $ \l -> lua_tothread l n

-- | If the value at the given index is a full userdata, returns its block
-- address. If the value is a light userdata, returns its pointer. Otherwise,
-- returns @nullPtr@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_touserdata lua_touserdata>.
touserdata :: StackIndex -> Lua (Ptr a)
touserdata n = liftLua $ \l -> lua_touserdata l n

-- | Returns the name of the type encoded by the value @tp@, which must be one
-- the values returned by @'ltype'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_typename lua_typename>.
typename :: Type -> Lua String
typename tp = liftLua $ \l ->
  lua_typename l (fromType tp) >>= peekCString

-- | Releases reference @'ref'@ from the table at index @idx@ (see @'ref'@). The
-- entry is removed from the table, so that the referred object can be
-- collected. The reference @'ref'@ is also freed to be used again.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -> Int -> Lua ()
unref idx r = liftLua $ \l ->
  luaL_unref l idx (fromIntegral r)

-- | Returns the pseudo-index that represents the @i@-th upvalue of the running
-- function (see <https://www.lua.org/manual/5.3/manual.html#4.4 §4.4> of the
-- Lua 5.3 reference manual).
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: StackIndex -> StackIndex
upvalueindex i = registryindex - i
