{-|
Module      : Foreign.Lua.Core.Functions
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Monadic functions which operate within the Lua type.

The functions in this module are mostly just thin wrappers around the respective
C functions. However, C function which can throw an error are wrapped such that
the error is converted into an @'Exception'@. Memory allocation errors,
however, are not caught and will cause the host program to terminate.
-}
module Foreign.Lua.Core.Functions where

import Prelude hiding (EQ, LT, compare, concat, error)

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Foreign.Lua.Core.Constants
import Foreign.Lua.Core.Error
import Foreign.Lua.Core.RawBindings
import Foreign.Lua.Core.Types as Lua
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.Storable as F

--
-- Helper functions
--

-- | Execute an action only if the given index is a table. Throw an error otherwise.
ensureTable :: StackIndex -> (Lua.State -> IO ()) -> Lua ()
ensureTable idx ioOp = do
  isTbl <- istable idx
  if isTbl
    then liftLua ioOp
    else do
      tyName <- ltype idx >>= typename
      throwException (Char8.pack "table expected, got " <> tyName)

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
-- Any error inside the called function cause a @'Exception'@ to be thrown.
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
  when (res /= OK) throwTopMessage

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
close :: Lua.State -> IO ()
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
  getfield registryindex hsluaErrorRegistryField
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
-- Errors on the Lua side are caught and rethrown as @'Exception'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_getfield lua_getfield>.
getfield :: StackIndex -> ByteString -> Lua ()
getfield i s = do
  absidx <- absindex i
  pushstring s
  gettable absidx

-- | Pushes onto the stack the value of the global @name@.
--
-- Errors on the Lua side are caught and rethrown as @'Exception'@.
--
-- Wrapper of
-- <https://www.lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>.
getglobal :: ByteString -> Lua ()
getglobal name = throwOnError <=< liftLua $ \l ->
  B.unsafeUseAsCStringLen name $ \(namePtr, len) ->
  hslua_getglobal l namePtr (fromIntegral len)

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
-- Errors on the Lua side are caught and rethrown as @'Exception'@.
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
-- (see @'Lua.Reader'@). The data argument is an opaque value passed to the
-- reader function.
--
-- The @chunkname@ argument gives a name to the chunk, which is used for error
-- messages and in debug information (see
-- <https://www.lua.org/manual/5.3/manual.html#4.9 §4.9>). Note that the
-- @chunkname@ is used as a C string, so it may not contain null-bytes.
load :: Lua.Reader -> Ptr () -> ByteString -> Lua Status
load reader data' chunkname = liftLua $ \l ->
  B.useAsCString chunkname $ \namePtr ->
  toStatus <$> lua_load l reader data' namePtr nullPtr

-- | Returns the type of the value in the given valid index, or @'TypeNone'@ for
-- a non-valid (but acceptable) index.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>.
ltype :: StackIndex -> Lua Type
ltype idx = toType <$> liftLua (flip lua_type idx)

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
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_next lua_next>.
next :: StackIndex -> Lua Bool
next idx = boolFromFailable =<< liftLua (\l -> hslua_next l idx)

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
pushinteger :: Lua.Integer -> Lua ()
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
pushnumber :: Lua.Number -> Lua ()
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
rawget n = ensureTable n (\l -> lua_rawget l n)

-- | Pushes onto the stack the value @t[n]@, where @t@ is the table at the given
-- index. The access is raw, that is, it does not invoke the @__index@
-- metamethod.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: StackIndex -> Lua.Integer -> Lua ()
rawgeti k n = ensureTable k (\l -> lua_rawgeti l k n)

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
rawset n = ensureTable n (\l -> lua_rawset l n)

-- | Does the equivalent of @t[i] = v@, where @t@ is the table at the given
-- index and @v@ is the value at the top of the stack.
--
-- This function pops the value from the stack. The assignment is raw, that is,
-- it does not invoke the @__newindex@ metamethod.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_rawseti lua_rawseti>.
rawseti :: StackIndex -> Lua.Integer -> Lua ()
rawseti k m = ensureTable k (\l -> lua_rawseti l k m)

-- | Sets the C function @f@ as the new value of global @name@.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_register lua_register>.
register :: ByteString -> CFunction -> Lua ()
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
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setfield lua_setfield>.
setfield :: StackIndex -> ByteString -> Lua ()
setfield i s = do
  absidx <- absindex i
  pushstring s
  insert (nthFromTop 2)
  settable absidx

-- | Pops a value from the stack and sets it as the new value of global @name@.
--
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setglobal lua_setglobal>.
setglobal :: ByteString -> Lua ()
setglobal name = throwOnError <=< liftLua $ \l ->
  B.unsafeUseAsCStringLen name $ \(namePtr, nameLen) ->
  hslua_setglobal l namePtr (fromIntegral nameLen)

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
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
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
-- function; otherwise, returns @Nothing@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: StackIndex -> Lua (Maybe CFunction)
tocfunction n = liftLua $ \l -> do
  fnPtr <- lua_tocfunction l n
  return (if fnPtr == nullFunPtr then Nothing else Just fnPtr)

-- | Converts the Lua value at the given acceptable index to the signed integral
-- type @'lua_Integer'@. The Lua value must be an integer, a number or a string
-- convertible to an integer (see
-- <https://www.lua.org/manual/5.3/manual.html#3.4.3 §3.4.3> of the Lua 5.3
-- Reference Manual); otherwise, @tointeger@ returns @Nothing@.
--
-- If the number is not an integer, it is truncated in some non-specified way.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tointeger lua_tointeger>.
tointeger :: StackIndex -> Lua (Maybe Lua.Integer)
tointeger n = liftLua $ \l -> alloca $ \boolPtr -> do
  res <- lua_tointegerx l n boolPtr
  isNum <- fromLuaBool <$> F.peek boolPtr
  return (if isNum then Just res else Nothing)

-- | Converts the Lua value at the given index to the C type lua_Number. The Lua
-- value must be a number or a string convertible to a number; otherwise,
-- @tonumber@ returns @'Nothing'@.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_tonumber lua_tonumber>.
tonumber :: StackIndex -> Lua (Maybe Lua.Number)
tonumber n = liftLua $ \l -> alloca $ \bptr -> do
  res <- lua_tonumberx l n bptr
  isNum <- fromLuaBool <$> F.peek bptr
  return (if isNum then Just res else Nothing)

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

-- | Converts the Lua value at the given index to a @'ByteString'@. The Lua
-- value must be a string or a number; otherwise, the function returns
-- @'Nothing'@. If the value is a number, then @'tostring'@ also changes the
-- actual value in the stack to a string. (This change confuses @'next'@ when
-- @'tostring'@ is applied to keys during a table traversal.)
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_tolstring lua_tolstring>.
tostring :: StackIndex -> Lua (Maybe ByteString)
tostring n = liftLua $ \l ->
  alloca $ \lenPtr -> do
    cstr <- lua_tolstring l n lenPtr
    if cstr == nullPtr
      then return Nothing
      else do
      cstrLen <- F.peek lenPtr
      Just <$> B.packCStringLen (cstr, fromIntegral cstrLen)

-- | Converts the value at the given index to a Lua thread (represented as
-- lua_State*). This value must be a thread; otherwise, the function returns
-- @Nothing@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tothread lua_tothread>.
tothread :: StackIndex -> Lua (Maybe Lua.State)
tothread n = liftLua $ \l -> do
  thread@(Lua.State ptr) <- lua_tothread l n
  if ptr == nullPtr
    then return Nothing
    else return (Just thread)

-- | If the value at the given index is a full userdata, returns its block
-- address. If the value is a light userdata, returns its pointer. Otherwise,
-- returns @Nothing@..
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_touserdata lua_touserdata>.
touserdata :: StackIndex -> Lua (Maybe (Ptr a))
touserdata n = liftLua $ \l -> do
  ptr <- lua_touserdata l n
  if ptr == nullPtr
    then return Nothing
    else return (Just ptr)

-- | Returns the name of the type encoded by the value @tp@, which must be one
-- the values returned by @'ltype'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_typename lua_typename>.
typename :: Type -> Lua ByteString
typename tp = liftLua $ \l ->
  lua_typename l (fromType tp) >>= B.unsafePackCString

-- | Returns the pseudo-index that represents the @i@-th upvalue of the running
-- function (see <https://www.lua.org/manual/5.3/manual.html#4.4 §4.4> of the
-- Lua 5.3 reference manual).
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: StackIndex -> StackIndex
upvalueindex i = registryindex - i
