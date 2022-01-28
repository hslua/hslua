{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Core.Primary
Copyright   : © 2007–2012 Gracjan Polak;
              © 2012–2016 Ömer Sinan Ağacan;
              © 2017-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : non-portable (depends on GHC)

Monadic functions which operate within the Lua type.

The functions in this module are mostly thin wrappers around the
respective C functions. However, C function which can throw an error are
wrapped such that the error is converted into an exception.
-}
module HsLua.Core.Primary where

import Prelude hiding (EQ, LT, compare, concat, error)

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import HsLua.Core.Error
import HsLua.Core.Types as Lua
import Lua
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.Storable as F

--
-- Helper functions
--

-- | Execute an action only if the given index is a table. Throw an
-- error otherwise.
ensureTable :: LuaError e => StackIndex -> (Lua.State -> IO a) -> LuaE e a
ensureTable idx ioOp = do
  isTbl <- istable idx
  if isTbl
    then liftLua ioOp
    else throwTypeMismatchError "table" idx
{-# INLINE ensureTable #-}

--
-- API functions
--

-- | Converts the acceptable index @idx@ into an equivalent absolute
-- index (that is, one that does not depend on the stack top).
--
-- Wraps 'lua_absindex'.
absindex :: StackIndex -> LuaE e StackIndex
absindex = liftLua1 lua_absindex
{-# INLINABLE absindex #-}

-- |  Calls a function.
--
-- To call a function you must use the following protocol: first, the
-- function to be called is pushed onto the stack; then, the arguments
-- to the function are pushed in direct order; that is, the first
-- argument is pushed first. Finally you call @call@; @nargs@ is the
-- number of arguments that you pushed onto the stack. All arguments and
-- the function value are popped from the stack when the function is
-- called. The function results are pushed onto the stack when the
-- function returns. The number of results is adjusted to @nresults@,
-- unless @nresults@ is @multret@. In this case, all results from the
-- function are pushed. Lua takes care that the returned values fit into
-- the stack space. The function results are pushed onto the stack in
-- direct order (the first result is pushed first), so that after the
-- call the last result is on the top of the stack.
--
-- Any error inside the called function is propagated as exception of
-- type @e@.
--
-- The following example shows how the host program can do the
-- equivalent to this Lua code:
--
-- > a = f("how", t.x, 14)
--
-- Here it is in Haskell (assuming the OverloadedStrings language
-- extension):
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
-- Note that the code above is "balanced": at its end, the stack is back
-- to its original configuration. This is considered good programming
-- practice.
--
-- See <https://www.lua.org/manual/5.3/manual.html#lua_call lua_call>.
call :: LuaError e => NumArgs -> NumResults -> LuaE e ()
call nargs nresults = do
  res <- pcall nargs nresults Nothing
  when (res /= OK) throwErrorAsException
{-# INLINABLE call #-}

-- | Ensures that the stack has space for at least @n@ extra slots (that
-- is, that you can safely push up to @n@ values into it). It returns
-- false if it cannot fulfill the request, either because it would cause
-- the stack to be larger than a fixed maximum size (typically at least
-- several thousand elements) or because it cannot allocate memory for
-- the extra space. This function never shrinks the stack; if the stack
-- already has space for the extra slots, it is left unchanged.
--
-- Wraps 'lua_checkstack'.
checkstack :: Int -> LuaE e Bool
checkstack n = liftLua $ \l ->
  fromLuaBool <$!> lua_checkstack l (fromIntegral n)
{-# INLINABLE checkstack #-}

-- | Destroys all objects in the given Lua state (calling the
-- corresponding garbage-collection metamethods, if any) and frees all
-- dynamic memory used by this state. On several platforms, you may not
-- need to call this function, because all resources are naturally
-- released when the host program ends. On the other hand, long-running
-- programs that create multiple states, such as daemons or web servers,
-- will probably need to close states as soon as they are not needed.
--
-- Same as 'lua_close'.
close :: Lua.State -> IO ()
close = lua_close
{-# INLINABLE close #-}

-- | Compares two Lua values. Returns 'True' if the value at index
-- @idx1@ satisfies @op@ when compared with the value at index @idx2@,
-- following the semantics of the corresponding Lua operator (that is,
-- it may call metamethods). Otherwise returns @False@. Also returns
-- @False@ if any of the indices is not valid.
--
-- The value of op must be of type 'RelationalOperator':
--
--    EQ: compares for equality (==)
--    LT: compares for less than (<)
--    LE: compares for less or equal (<=)
--
-- Wraps 'hslua_compare'. See also
-- <https://www.lua.org/manual/5.3/manual.html#lua_compare lua_compare>.
compare :: LuaError e
        => StackIndex         -- ^ idx1
        -> StackIndex         -- ^ idx2
        -> RelationalOperator
        -> LuaE e Bool
compare idx1 idx2 relOp = fromLuaBool <$!> liftLuaThrow
  (\l -> hslua_compare l idx1 idx2 (fromRelationalOperator relOp))
{-# INLINABLE compare #-}

-- | Concatenates the @n@ values at the top of the stack, pops them, and
-- leaves the result at the top. If @n@ is 1, the result is the single
-- value on the stack (that is, the function does nothing); if @n@ is 0,
-- the result is the empty string. Concatenation is performed following
-- the usual semantics of Lua (see
-- <https://www.lua.org/manual/5.3/manual.html#3.4.6 §3.4.6> of the Lua
-- manual).
--
-- Wraps 'hslua_concat'. See also
-- <https://www.lua.org/manual/5.3/manual.html#lua_concat lua_concat>.
concat :: LuaError e => NumArgs -> LuaE e ()
concat n = liftLuaThrow (`hslua_concat` n)
{-# INLINABLE concat #-}

-- | Copies the element at index @fromidx@ into the valid index @toidx@,
-- replacing the value at that position. Values at other positions are
-- not affected.
--
-- Wraps 'lua_copy'.
copy :: StackIndex -> StackIndex -> LuaE e ()
copy fromidx toidx = liftLua $ \l -> lua_copy l fromidx toidx
{-# INLINABLE copy #-}

-- | Creates a new empty table and pushes it onto the stack. Parameter
-- narr is a hint for how many elements the table will have as a
-- sequence; parameter nrec is a hint for how many other elements the
-- table will have. Lua may use these hints to preallocate memory for
-- the new table. This preallocation is useful for performance when you
-- know in advance how many elements the table will have. Otherwise you
-- can use the function lua_newtable.
--
-- Wraps 'lua_createtable'.
createtable :: Int -> Int -> LuaE e ()
createtable narr nrec = liftLua $ \l ->
  lua_createtable l (fromIntegral narr) (fromIntegral nrec)
{-# INLINABLE createtable #-}

-- TODO: implement dump

-- | Returns @True@ if the two values in acceptable indices @index1@ and
-- @index2@ are equal, following the semantics of the Lua @==@ operator
-- (that is, may call metamethods). Otherwise returns @False@. Also
-- returns @False@ if any of the indices is non valid. Uses @'compare'@
-- internally.
equal :: LuaError e
      => StackIndex  -- ^ index1
      -> StackIndex  -- ^ index2
      -> LuaE e Bool
equal index1 index2 = compare index1 index2 EQ
{-# INLINABLE equal #-}

-- | Signals to Lua that an error has occurred and that the error object
-- is at the top of the stack.
error :: LuaE e NumResults
error = liftLua hslua_error
{-# INLINABLE error #-}

-- |  Controls the garbage collector.
--
-- This function performs several tasks, according to the given control
-- command. See the documentation for 'GCControl'.
--
-- Wraps 'lua_gc'.
gc :: GCControl -> LuaE e Int
gc what = liftLua $ \l ->
  fromIntegral <$!> lua_gc l (toGCcode what) (toGCdata what)
{-# INLINABLE gc #-}

-- | Pushes onto the stack the value @t[k]@, where @t@ is the value at
-- the given stack index. As in Lua, this function may trigger a
-- metamethod for the "index" event (see
-- <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of Lua's
-- manual).
--
-- Errors on the Lua side are propagated.
--
-- See also
-- <https://www.lua.org/manual/5.3/manual.html#lua_getfield lua_getfield>.
getfield :: LuaError e => StackIndex -> Name -> LuaE e Type
getfield i (Name s) = do
  absidx <- absindex i
  pushstring s
  gettable absidx
{-# INLINABLE getfield #-}

-- | Pushes onto the stack the value of the global @name@.
--
-- Errors on the Lua side are propagated.
--
-- Wraps 'hslua_getglobal'.
getglobal :: LuaError e => Name -> LuaE e Type
getglobal (Name name) = liftLuaThrow $ \l status' ->
  B.unsafeUseAsCStringLen name $ \(namePtr, len) ->
  toType <$!> hslua_getglobal l namePtr (fromIntegral len) status'
{-# INLINABLE getglobal #-}

-- | If the value at the given index has a metatable, the function
-- pushes that metatable onto the stack and returns @True@. Otherwise,
-- the function returns @False@ and pushes nothing on the stack.
--
-- Wraps 'lua_getmetatable'.
getmetatable :: StackIndex -> LuaE e Bool
getmetatable n = liftLua $ \l ->
  fromLuaBool <$!> lua_getmetatable l n
{-# INLINABLE getmetatable #-}

-- | Pushes onto the stack the value @t[k]@, where @t@ is the value at
-- the given index and @k@ is the value at the top of the stack.
--
-- This function pops the key from the stack, pushing the resulting
-- value in its place. As in Lua, this function may trigger a metamethod
-- for the "index" event (see
-- <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of Lua's
-- manual).
--
-- Errors on the Lua side are caught and rethrown.
--
-- Wraps 'hslua_gettable'. See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>.
gettable :: LuaError e => StackIndex -> LuaE e Type
gettable n = liftLuaThrow (\l -> fmap toType . hslua_gettable l n)
{-# INLINABLE gettable #-}

-- | Returns the index of the top element in the stack. Because indices
-- start at 1, this result is equal to the number of elements in the
-- stack (and so 0 means an empty stack).
--
-- Wraps 'lua_gettop'.
gettop :: LuaE e StackIndex
gettop = liftLua lua_gettop
{-# INLINABLE gettop #-}

-- | Pushes onto the stack the Lua value associated with the full
-- userdata at the given index.
--
-- Returns the type of the pushed value.
--
-- Wraps 'lua_getuservalue'.
getuservalue :: StackIndex -> LuaE e Type
getuservalue idx = toType <$> liftLua (`lua_getuservalue` idx)

-- | Moves the top element into the given valid index, shifting up the
-- elements above this index to open space. This function cannot be
-- called with a pseudo-index, because a pseudo-index is not an actual
-- stack position.
--
-- Wraps 'lua_insert'.
insert :: StackIndex -> LuaE e ()
insert index = liftLua $ \l -> lua_insert l index
{-# INLINABLE insert #-}

-- | Returns 'True' if the value at the given index is a boolean, and
-- 'False' otherwise.
--
-- Wraps 'lua_isboolean'.
isboolean :: StackIndex -> LuaE e Bool
isboolean n = liftLua $ \l -> fromLuaBool <$!> lua_isboolean l n
{-# INLINABLE isboolean #-}

-- | Returns 'True' if the value at the given index is a C function, and
-- 'False' otherwise.
--
-- Wraps 'lua_iscfunction'.
iscfunction :: StackIndex -> LuaE e Bool
iscfunction n = liftLua $ \l -> fromLuaBool <$!> lua_iscfunction l n
{-# INLINABLE iscfunction #-}

-- | Returns 'True' if the value at the given index is a function
-- (either C or Lua), and 'False' otherwise.
--
-- Wraps 'lua_isfunction'.
isfunction :: StackIndex -> LuaE e Bool
isfunction n = liftLua $ \l -> fromLuaBool <$!> lua_isfunction l n
{-# INLINABLE isfunction #-}

-- | Returns @True@ if the value at the given index is an integer (that
-- is, the value is a number and is represented as an integer), and
-- 'False' otherwise.
--
-- Wraps 'lua_isinteger'.
isinteger :: StackIndex -> LuaE e Bool
isinteger n = liftLua $ \l -> fromLuaBool <$!> lua_isinteger l n
{-# INLINABLE isinteger #-}

-- | Returns @True@ if the value at the given index is a light userdata,
-- and @False@ otherwise.
--
-- Wraps 'lua_islightuserdata'.
islightuserdata :: StackIndex -> LuaE e Bool
islightuserdata n = liftLua $ \l -> fromLuaBool <$!> lua_islightuserdata l n
{-# INLINABLE islightuserdata #-}

-- | Returns 'True' if the value at the given index is *nil*, and
-- 'False' otherwise.
--
-- Wraps 'lua_isnil'.
isnil :: StackIndex -> LuaE e Bool
isnil n = liftLua $ \l -> fromLuaBool <$!> lua_isnil l n
{-# INLINABLE isnil #-}

-- | Returns 'True' if the given index is not valid, and 'False'
-- otherwise.
--
-- Wraps 'lua_isnone'.
isnone :: StackIndex -> LuaE e Bool
isnone n = liftLua $ \l -> fromLuaBool <$!> lua_isnone l n
{-# INLINABLE isnone #-}

-- | Returns 'True' if the given index is not valid or if the value at
-- the given index is *nil*, and 'False' otherwise.
--
-- Wraps 'lua_isnoneornil'.
isnoneornil :: StackIndex -> LuaE e Bool
isnoneornil n = liftLua $ \l -> fromLuaBool <$!> lua_isnoneornil l n
{-# INLINABLE isnoneornil #-}

-- | Returns 'True' if the value at the given index is a number or a
-- string convertible to a number, and 'False' otherwise.
--
-- Wraps 'lua_isnumber'.
isnumber :: StackIndex -> LuaE e Bool
isnumber n = liftLua $ \l -> fromLuaBool <$!> lua_isnumber l n
{-# INLINABLE isnumber #-}

-- | Returns 'True' if the value at the given index is a string or a
-- number (which is always convertible to a string), and 'False'
-- otherwise.
--
-- Wraps 'lua_isstring'.
isstring :: StackIndex -> LuaE e Bool
isstring n = liftLua $ \l -> fromLuaBool <$!> lua_isstring l n
{-# INLINABLE isstring #-}

-- | Returns 'True' if the value at the given index is a table, and
-- 'False' otherwise.
--
-- Wraps 'lua_istable'.
istable :: StackIndex -> LuaE e Bool
istable n = liftLua $ \l -> fromLuaBool <$!> lua_istable l n
{-# INLINABLE istable #-}

-- | Returns 'True' if the value at the given index is a thread, and
-- 'False' otherwise.
--
-- Wraps 'lua_isthread'.
isthread :: StackIndex -> LuaE e Bool
isthread n = liftLua $ \l -> fromLuaBool <$!> lua_isthread l n
{-# INLINABLE isthread #-}

-- | Returns 'True' if the value at the given index is a userdata
-- (either full or light), and 'False' otherwise.
--
-- Wraps 'lua_isuserdata'.
isuserdata :: StackIndex -> LuaE e Bool
isuserdata n = liftLua $ \l -> fromLuaBool <$!> lua_isuserdata l n
{-# INLINABLE isuserdata #-}

-- | Tests whether the object under the first index is smaller than that
-- under the second. Uses @'compare'@ internally.
lessthan :: LuaError e =>  StackIndex -> StackIndex -> LuaE e Bool
lessthan index1 index2 = compare index1 index2 LT
{-# INLINABLE lessthan #-}

-- | Loads a Lua chunk (without running it). If there are no errors,
-- @'load'@ pushes the compiled chunk as a Lua function on top of the
-- stack. Otherwise, it pushes an error message.
--
-- The return values of @'load'@ are:
--
-- - @'OK'@: no errors;
-- - @'ErrSyntax'@: syntax error during pre-compilation;
-- - @'ErrMem'@: memory allocation error;
-- - @'ErrGcmm'@: error while running a @__gc@ metamethod. (This error
--   has no relation with the chunk being loaded. It is generated by the
--   garbage collector.)
--
-- This function only loads a chunk; it does not run it.
--
-- @load@ automatically detects whether the chunk is text or binary, and
-- loads it accordingly (see program luac).
--
-- The @'load'@ function uses a user-supplied reader function to read
-- the chunk (see @'Lua.Reader'@). The data argument is an opaque value
-- passed to the reader function.
--
-- The @chunkname@ argument gives a name to the chunk, which is used for
-- error messages and in debug information (see
-- <https://www.lua.org/manual/5.3/manual.html#4.9 §4.9>). Note that the
-- @chunkname@ is used as a C string, so it may not contain null-bytes.
--
-- This is a wrapper of 'lua_load'.
load :: Lua.Reader -> Ptr () -> Name -> LuaE e Status
load reader data' (Name chunkname) = liftLua $ \l ->
  B.useAsCString chunkname $ \namePtr ->
  toStatus <$!> lua_load l reader data' namePtr nullPtr
{-# INLINABLE load #-}

-- | Returns the type of the value in the given valid index, or
-- @'TypeNone'@ for a non-valid (but acceptable) index.
--
-- This function wraps 'lua_type'.
ltype :: StackIndex -> LuaE e Type
ltype idx = toType <$!> liftLua (`lua_type` idx)
{-# INLINABLE ltype #-}

-- | Creates a new empty table and pushes it onto the stack. It is
-- equivalent to @createtable 0 0@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_newtable lua_newtable>.
newtable :: LuaE e ()
newtable = createtable 0 0
{-# INLINABLE newtable #-}

-- | This function allocates a new block of memory with the given size,
-- pushes onto the stack a new full userdata with the block address, and
-- returns this address. The host program can freely use this memory.
--
-- This function wraps 'lua_newuserdata'.
newuserdata :: Int -> LuaE e (Ptr ())
newuserdata = liftLua1 lua_newuserdata . fromIntegral
{-# INLINABLE newuserdata #-}

-- | Pops a key from the stack, and pushes a key–value pair from the
-- table at the given index (the "next" pair after the given key). If
-- there are no more elements in the table, then @next@ returns @False@
-- (and pushes nothing).
--
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
--
-- This function wraps 'hslua_next'.
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_next lua_next>.
next :: LuaError e => StackIndex -> LuaE e Bool
next idx = fromLuaBool <$!> liftLuaThrow (\l -> hslua_next l idx)
{-# INLINABLE next #-}

-- | Opens all standard Lua libraries into the current state and sets
-- each library name as a global value.
--
-- This function wraps 'luaL_openlibs'.
openlibs :: LuaE e ()
openlibs = liftLua luaL_openlibs
{-# INLINABLE openlibs #-}

-- | Pushes Lua's /base/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_base'.
openbase :: LuaError e => LuaE e ()
openbase = pushcfunction luaopen_base *> call 0 multret
{-# INLINABLE openbase #-}

-- | Pushes Lua's /debug/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_io'.
opendebug :: LuaError e => LuaE e ()
opendebug = pushcfunction luaopen_debug *> call 0 multret
{-# INLINABLE opendebug #-}

-- | Pushes Lua's /io/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_io'.
openio :: LuaError e => LuaE e ()
openio = pushcfunction luaopen_io *> call 0 multret
{-# INLINABLE openio #-}

-- | Pushes Lua's /math/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_math'.
openmath :: LuaError e => LuaE e ()
openmath = pushcfunction luaopen_math *> call 0 multret
{-# INLINABLE openmath #-}

-- | Pushes Lua's /os/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_os'.
openos :: LuaError e => LuaE e ()
openos = pushcfunction luaopen_os *> call 0 multret
{-# INLINABLE openos #-}

-- | Pushes Lua's /package/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_package'.
openpackage :: LuaError e => LuaE e ()
openpackage = pushcfunction luaopen_package *> call 0 multret
{-# INLINABLE openpackage #-}

-- | Pushes Lua's /string/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_string'.
openstring :: LuaError e => LuaE e ()
openstring = pushcfunction luaopen_string *> call 0 multret
{-# INLINABLE openstring #-}

-- | Pushes Lua's /table/ library onto the stack.
--
-- This function pushes and and calls 'luaopen_table'.
opentable :: LuaError e => LuaE e ()
opentable = pushcfunction luaopen_table *> call 0 multret
{-# INLINABLE opentable #-}

-- | Calls a function in protected mode.
--
-- Both @nargs@ and @nresults@ have the same meaning as in @'call'@. If
-- there are no errors during the call, @pcall@ behaves exactly like
-- @'call'@. However, if there is any error, @pcall@ catches it, pushes
-- a single value on the stack (the error message), and returns the
-- error code. Like @'call'@, @pcall@ always removes the function and
-- its arguments from the stack.
--
-- If @msgh@ is @Nothing@, then the error object returned on the stack
-- is exactly the original error object. Otherwise, when @msgh@ is @Just
-- idx@, the stack index @idx@ is the location of a message handler.
-- (This index cannot be a pseudo-index.) In case of runtime errors,
-- this function will be called with the error object and its return
-- value will be the object returned on the stack by @'pcall'@.
--
-- Typically, the message handler is used to add more debug information
-- to the error object, such as a stack traceback. Such information
-- cannot be gathered after the return of @'pcall'@, since by then the
-- stack has unwound.
--
-- This function wraps 'lua_pcall'.
pcall :: NumArgs -> NumResults -> Maybe StackIndex -> LuaE e Status
pcall nargs nresults msgh = liftLua $ \l ->
  toStatus <$!> lua_pcall l nargs nresults (fromMaybe 0 msgh)
{-# INLINABLE pcall #-}

-- | Pops @n@ elements from the stack.
--
-- See also: <https://www.lua.org/manual/5.3/manual.html#lua_pop lua_pop>.
pop :: Int -> LuaE e ()
pop n = liftLua $ \l -> lua_pop l (fromIntegral n)
{-# INLINABLE pop #-}

-- | Pushes a boolean value with the given value onto the stack.
--
-- This functions wraps 'lua_pushboolean'.
pushboolean :: Bool -> LuaE e ()
pushboolean b = liftLua $ \l -> lua_pushboolean l (toLuaBool b)
{-# INLINABLE pushboolean #-}

-- | Pushes a new C closure onto the stack.
--
-- When a C function is created, it is possible to associate some values
-- with it, thus creating a C closure (see
-- <https://www.lua.org/manual/5.1/manual.html#3.4 §3.4>); these values
-- are then accessible to the function whenever it is called. To
-- associate values with a C function, first these values should be
-- pushed onto the stack (when there are multiple values, the first
-- value is pushed first). Then pushcclosure is called to create and
-- push the C function onto the stack, with the argument @n@ telling how
-- many values should be associated with the function. pushcclosure also
-- pops these values from the stack.
--
-- The maximum value for @n@ is 255.
--
-- Wraps 'lua_pushcclosure'.
pushcclosure :: CFunction -> NumArgs {- ^ n -} -> LuaE e ()
pushcclosure f n = liftLua $ \l -> lua_pushcclosure l f n
{-# INLINABLE pushcclosure #-}

-- | Pushes a C function onto the stack. This function receives a
-- pointer to a C function and pushes onto the stack a Lua value of type
-- function that, when called, invokes the corresponding C function.
--
-- Any function to be callable by Lua must follow the correct protocol
-- to receive its parameters and return its results (see @'CFunction'@)
--
-- Same as @flip 'pushcclosure' 0@.
-- <https://www.lua.org/manual/5.3/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: CFunction -> LuaE e ()
pushcfunction f = pushcclosure f 0
{-# INLINABLE pushcfunction #-}

-- | Pushes the global environment onto the stack.
--
-- Wraps 'lua_pushglobaltable'.
pushglobaltable :: LuaE e ()
pushglobaltable = liftLua lua_pushglobaltable
{-# INLINABLE pushglobaltable #-}

-- | Pushes an integer with with the given value onto the stack.
--
-- Wraps 'lua_pushinteger'.
pushinteger :: Lua.Integer -> LuaE e ()
pushinteger = liftLua1 lua_pushinteger
{-# INLINABLE pushinteger #-}

-- |  Pushes a light userdata onto the stack.
--
-- Userdata represent C values in Lua. A light userdata represents a
-- pointer, a @Ptr a@ (i.e., @void*@ in C). It is a value (like a
-- number): you do not create it, it has no individual metatable, and it
-- is not collected (as it was never created). A light userdata is equal
-- to "any" light userdata with the same C address.
--
-- Wraps 'lua_pushlightuserdata'.
pushlightuserdata :: Ptr a -> LuaE e ()
pushlightuserdata = liftLua1 lua_pushlightuserdata
{-# INLINABLE pushlightuserdata #-}

-- | Pushes a nil value onto the stack.
--
-- Wraps 'lua_pushnil'.
pushnil :: LuaE e ()
pushnil = liftLua lua_pushnil
{-# INLINABLE pushnil #-}

-- | Pushes a float with the given value onto the stack.
--
-- Wraps 'lua_pushnumber'.
pushnumber :: Lua.Number -> LuaE e ()
pushnumber = liftLua1 lua_pushnumber
{-# INLINABLE pushnumber #-}

-- | Pushes the string pointed to by s onto the stack. Lua makes (or
-- reuses) an internal copy of the given string, so the memory at s can
-- be freed or reused immediately after the function returns.
--
-- Wraps 'lua_pushlstring'.
pushstring :: ByteString -> LuaE e ()
pushstring s = liftLua $ \l ->
  B.unsafeUseAsCStringLen s $ \(sPtr, z) -> lua_pushlstring l sPtr (fromIntegral z)
{-# INLINABLE pushstring #-}

-- | Pushes the current thread onto the stack. Returns @True@ if this thread is
-- the main thread of its state, @False@ otherwise.
--
-- Wraps 'lua_pushthread'.
pushthread :: LuaE e Bool
pushthread = (1 ==)  <$!> liftLua lua_pushthread
{-# INLINABLE pushthread #-}

-- | Pushes a copy of the element at the given index onto the stack.
--
-- Wraps 'lua_pushvalue'.
pushvalue :: StackIndex -> LuaE e ()
pushvalue n = liftLua $ \l -> lua_pushvalue l n
{-# INLINABLE pushvalue #-}

-- | Returns @True@ if the two values in indices @idx1@ and @idx2@ are
-- primitively equal (that is, without calling the @__eq@ metamethod).
-- Otherwise returns @False@. Also returns @False@ if any of the indices
-- are not valid.
--
-- Wraps 'lua_rawequal'.
rawequal :: StackIndex -> StackIndex -> LuaE e Bool
rawequal idx1 idx2 = liftLua $ \l ->
  fromLuaBool <$!> lua_rawequal l idx1 idx2
{-# INLINABLE rawequal #-}

-- | Similar to @'gettable'@, but does a raw access (i.e., without
-- metamethods).
--
-- Wraps 'lua_rawget'.
rawget :: LuaError e => StackIndex -> LuaE e Type
rawget n = ensureTable n (\l -> toType <$!> lua_rawget l n)
{-# INLINABLE rawget #-}

-- | Pushes onto the stack the value @t[n]@, where @t@ is the table at
-- the given index. The access is raw, that is, it does not invoke the
-- @__index@ metamethod.
--
-- Wraps 'lua_rawgeti'.
rawgeti :: LuaError e => StackIndex -> Lua.Integer -> LuaE e Type
rawgeti k n = ensureTable k (\l -> toType <$!> lua_rawgeti l k n)
{-# INLINABLE rawgeti #-}

-- | Returns the raw "length" of the value at the given index: for
-- strings, this is the string length; for tables, this is the result of
-- the length operator (@#@) with no metamethods; for userdata, this is
-- the size of the block of memory allocated for the userdata; for other
-- values, it is 0.
--
-- Wraps 'lua_rawlen'.
rawlen :: StackIndex -> LuaE e Int
rawlen idx = liftLua $ \l -> fromIntegral <$!> lua_rawlen l idx
{-# INLINABLE rawlen #-}

-- | Similar to @'settable'@, but does a raw assignment (i.e., without
-- metamethods).
--
-- Wraps 'lua_rawset'.
rawset :: LuaError e => StackIndex -> LuaE e ()
rawset n = ensureTable n (\l -> lua_rawset l n)
{-# INLINABLE rawset #-}

-- | Does the equivalent of @t[i] = v@, where @t@ is the table at the given
-- index and @v@ is the value at the top of the stack.
--
-- This function pops the value from the stack. The assignment is raw, that is,
-- it does not invoke the @__newindex@ metamethod.
--
-- Wraps 'lua_rawseti'.
rawseti :: LuaError e => StackIndex -> Lua.Integer -> LuaE e ()
rawseti k m = ensureTable k (\l -> lua_rawseti l k m)
{-# INLINABLE rawseti #-}

-- | Sets the C function @f@ as the new value of global @name@.
--
-- Behaves like "lua_register".
register :: LuaError e => Name -> CFunction -> LuaE e ()
register name f = do
  pushcfunction f
  setglobal name
{-# INLINABLE register #-}

-- | Removes the element at the given valid index, shifting down the
-- elements above this index to fill the gap. This function cannot be
-- called with a pseudo-index, because a pseudo-index is not an actual
-- stack position.
--
-- Wraps 'lua_remove'.
remove :: StackIndex -> LuaE e ()
remove n = liftLua $ \l -> lua_remove l n
{-# INLINABLE remove #-}

-- | Moves the top element into the given valid index without shifting
-- any element (therefore replacing the value at that given index), and
-- then pops the top element.
--
-- Wraps 'lua_replace'.
replace :: StackIndex -> LuaE e ()
replace n = liftLua $ \l ->  lua_replace l n
{-# INLINABLE replace #-}

-- | Rotates the stack elements between the valid index @idx@ and the
-- top of the stack. The elements are rotated @n@ positions in the
-- direction of the top, for a positive @n@, or @-n@ positions in the
-- direction of the bottom, for a negative @n@. The absolute value of
-- @n@ must not be greater than the size of the slice being rotated.
-- This function cannot be called with a pseudo-index, because a
-- pseudo-index is not an actual stack position.
--
-- <https://www.lua.org/manual/5.3/manual.html#lua_rotate>
rotate :: StackIndex  -- ^ @idx@
       -> Int         -- ^ @n@
       -> LuaE e ()
rotate idx n = liftLua $ \l -> lua_rotate l idx (fromIntegral n)
{-# INLINABLE rotate #-}

-- | Does the equivalent to @t[k] = v@, where @t@ is the value at the
-- given index and @v@ is the value at the top of the stack.
--
-- This function pops the value from the stack. As in Lua, this function
-- may trigger a metamethod for the "newindex" event (see
-- <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of the Lua 5.3
-- Reference Manual).
--
-- Errors on the Lua side are caught and rethrown as a @'Exception'@.
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setfield lua_setfield>.
setfield :: LuaError e => StackIndex -> Name -> LuaE e ()
setfield i (Name s) = do
  absidx <- absindex i
  pushstring s
  insert (nthTop 2)
  settable absidx
{-# INLINABLE setfield #-}

-- | Pops a value from the stack and sets it as the new value of global
-- @name@.
--
-- Errors on the Lua side are caught and rethrown as 'Exception'.
--
-- Wraps 'hslua_setglobal'. See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_setglobal lua_setglobal>.
setglobal :: LuaError e => Name {- ^ name -} -> LuaE e ()
setglobal (Name name) = liftLuaThrow $ \l status' ->
  B.unsafeUseAsCStringLen name $ \(namePtr, nameLen) ->
    hslua_setglobal l namePtr (fromIntegral nameLen) status'
{-# INLINABLE setglobal #-}

-- | Pops a table from the stack and sets it as the new metatable for
-- the value at the given index.
--
-- Wraps 'lua_setmetatable'.
setmetatable :: StackIndex -> LuaE e ()
setmetatable idx = liftLua $ \l -> lua_setmetatable l idx
{-# INLINABLE setmetatable #-}

-- | Does the equivalent to @t[k] = v@, where @t@ is the value at the
-- given index, @v@ is the value at the top of the stack, and @k@ is the
-- value just below the top.
--
-- This function pops both the key and the value from the stack. As in
-- Lua, this function may trigger a metamethod for the "newindex" event
-- (see <https://www.lua.org/manual/5.3/manual.html#2.4 §2.4> of the Lua
-- 5.3 Reference Manual).
--
-- Errors on the Lua side are caught and rethrown.
--
-- Wraps 'hslua_settable'.
settable :: LuaError e => StackIndex -> LuaE e ()
settable index = liftLuaThrow $ \l -> hslua_settable l index
{-# INLINABLE settable #-}

-- | Accepts any index, or 0, and sets the stack top to this index. If
-- the new top is larger than the old one, then the new elements are
-- filled with nil. If index is 0, then all stack elements are removed.
--
-- Wraps 'lua_settop'.
settop :: StackIndex -> LuaE e ()
settop = liftLua1 lua_settop
{-# INLINABLE settop #-}

-- | Pops a value from the stack and sets it as the new value associated
-- to the full userdata at the given index.
--
-- <https://www.lua.org/manual/5.3/manual.html#lua_setuservalue>
setuservalue :: StackIndex -> LuaE e ()
setuservalue idx = liftLua (`lua_setuservalue` idx)

-- |  Returns the status of this Lua thread.
--
-- The status can be 'OK' for a normal thread, an error value if the
-- thread finished the execution of a @lua_resume@ with an error, or
-- 'Yield' if the thread is suspended.
--
-- You can only call functions in threads with status 'OK'. You can
-- resume threads with status 'OK' (to start a new coroutine) or 'Yield'
-- (to resume a coroutine).
--
-- Wraps 'lua_status'.
status :: LuaE e Status
status = liftLua $ fmap toStatus . lua_status
{-# INLINABLE status #-}

-- | Converts the Lua value at the given index to a haskell boolean
-- value. Like all tests in Lua, @toboolean@ returns @True@ for any Lua
-- value different from @false@ and @nil@; otherwise it returns @False@.
-- (If you want to accept only actual boolean values, use @'isboolean'@
-- to test the value's type.)
--
-- Wraps 'lua_toboolean'.
toboolean :: StackIndex -> LuaE e Bool
toboolean n = liftLua $ \l -> fromLuaBool <$!> lua_toboolean l n
{-# INLINABLE toboolean #-}

-- | Converts a value at the given index to a C function. That value
-- must be a C function; otherwise, returns @Nothing@.
--
-- Wraps 'lua_tocfunction'.
tocfunction :: StackIndex -> LuaE e (Maybe CFunction)
tocfunction n = liftLua $ \l -> do
  fnPtr <- lua_tocfunction l n
  return (if fnPtr == nullFunPtr then Nothing else Just fnPtr)
{-# INLINABLE tocfunction #-}

-- | Converts the Lua value at the given acceptable index to the signed
-- integral type 'Lua.Integer'. The Lua value must be an integer, a
-- number or a string convertible to an integer (see
-- <https://www.lua.org/manual/5.3/manual.html#3.4.3 §3.4.3> of the Lua
-- 5.3 Reference Manual); otherwise, @tointeger@ returns @Nothing@.
--
-- If the number is not an integer, it is truncated in some
-- non-specified way.
--
-- Wraps 'lua_tointegerx'. See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_tointeger lua_tointeger>.
tointeger :: StackIndex -> LuaE e (Maybe Lua.Integer)
tointeger n = liftLua $ \l -> alloca $ \boolPtr -> do
  res <- lua_tointegerx l n boolPtr
  isNum <- fromLuaBool <$!> F.peek boolPtr
  return (if isNum then Just res else Nothing)
{-# INLINABLE tointeger #-}

-- | Converts the Lua value at the given index to a 'Lua.Number'. The
-- Lua value must be a number or a string convertible to a number;
-- otherwise, @tonumber@ returns @'Nothing'@.
--
-- Wraps 'lua_tonumberx'. See also
-- <https://www.lua.org/manual/5.3/manual.html#lua_tonumber lua_tonumber>.
tonumber :: StackIndex -> LuaE e (Maybe Lua.Number)
tonumber n = liftLua $ \l -> alloca $ \bptr -> do
  res <- lua_tonumberx l n bptr
  isNum <- fromLuaBool <$!> F.peek bptr
  return (if isNum then Just res else Nothing)
{-# INLINABLE tonumber #-}

-- | Converts the value at the given index to a generic C pointer
-- (void*). The value can be a userdata, a table, a thread, or a
-- function; otherwise, lua_topointer returns @nullPtr@. Different
-- objects will give different pointers. There is no way to convert the
-- pointer back to its original value.
--
-- Typically this function is used only for hashing and debug
-- information.
--
-- Wraps 'lua_topointer'.
topointer :: StackIndex -> LuaE e (Ptr ())
topointer n = liftLua $ \l -> lua_topointer l n
{-# INLINABLE topointer #-}

-- | Converts the Lua value at the given index to a 'ByteString'. The
-- Lua value must be a string or a number; otherwise, the function
-- returns 'Nothing'. If the value is a number, then 'tostring' also
-- changes the actual value in the stack to a string. (This change
-- confuses 'next' when 'tostring' is applied to keys during a table
-- traversal.)
--
-- Wraps 'lua_tolstring'.
tostring :: StackIndex -> LuaE e (Maybe ByteString)
tostring n = liftLua $ \l ->
  alloca $ \lenPtr -> do
    cstr <- lua_tolstring l n lenPtr
    if cstr == nullPtr
      then return Nothing
      else do
      cstrLen <- F.peek lenPtr
      Just <$!> B.packCStringLen (cstr, fromIntegral cstrLen)
{-# INLINABLE tostring #-}

-- | Converts the value at the given index to a Lua thread (represented
-- as 'Lua.State'). This value must be a thread; otherwise, the function
-- returns @Nothing@.
--
-- Wraps 'lua_tothread'.
tothread :: StackIndex -> LuaE e (Maybe Lua.State)
tothread n = liftLua $ \l -> do
  thread@(Lua.State ptr) <- lua_tothread l n
  if ptr == nullPtr
    then return Nothing
    else return (Just thread)
{-# INLINABLE tothread #-}

-- | If the value at the given index is a full userdata, returns its
-- block address. If the value is a light userdata, returns its pointer.
-- Otherwise, returns @Nothing@..
--
-- Wraps 'lua_touserdata'.
touserdata :: StackIndex -> LuaE e (Maybe (Ptr a))
touserdata n = liftLua $ \l -> do
  ptr <- lua_touserdata l n
  if ptr == nullPtr
    then return Nothing
    else return (Just ptr)
{-# INLINABLE touserdata #-}

-- | Returns the name of the type encoded by the value @tp@, which must
-- be one the values returned by @'ltype'@.
--
-- Wraps 'lua_typename'.
typename :: Type -> LuaE e ByteString
typename tp = liftLua $ \l ->
  lua_typename l (fromType tp) >>= B.packCString
{-# INLINABLE typename #-}

-- | Returns the pseudo-index that represents the @i@-th upvalue of the
-- running function (see <https://www.lua.org/manual/5.3/manual.html#4.4
-- §4.4> of the Lua 5.3 reference manual).
--
-- See also:
-- <https://www.lua.org/manual/5.3/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: StackIndex -> StackIndex
upvalueindex i = registryindex - i
{-# INLINABLE upvalueindex #-}
