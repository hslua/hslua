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
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Foreign.Lua.Functions
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : CPP, ForeignFunctionInterface

Monadic functions which operate within the Lua type.
-}
module Foreign.Lua.Functions where

import Prelude hiding (compare, concat)

import Control.Monad
import Foreign.C
import Foreign.Lua.Constants
import Foreign.Lua.RawBindings
import Foreign.Lua.Types.Core
import Foreign.Marshal.Alloc
import Foreign.Ptr

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Foreign.Storable as F

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_atpanic lua_atpanic>.
atpanic :: FunPtr LuaCFunction -> Lua (FunPtr LuaCFunction)
atpanic = liftLua1 lua_atpanic

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_call lua_call>.
call :: NumArgs -> NumResults -> Lua ()
#if LUA_VERSION_NUMBER >= 502
call a nresults = liftLua $ \l ->
  lua_callk l (fromNumArgs a) (fromNumResults nresults) 0 nullPtr
#else
call a nresults = liftLua $ \l ->
  lua_call l (fromNumArgs a) (fromNumResults nresults)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_checkstack lua_checkstack>.
checkstack :: Int -> Lua Bool
checkstack n = liftLua $ \l -> liftM (/= 0) (lua_checkstack l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_close lua_close>.
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
-- See <https://www.lua.org/manual/5.3/manual.html#lua_compare lua_compare>.
compare :: StackIndex -> StackIndex -> LuaComparerOp -> Lua Bool
#if LUA_VERSION_NUMBER >= 502
compare idx1 idx2 op = liftLua $ \l -> (/= 0) <$>
  lua_compare l
    (fromStackIndex idx1)
    (fromStackIndex idx2)
    (fromIntegral (fromEnum op))
#else
compare idx1 idx2 op = liftLua $ \l ->
  (/= 0) <$>
  case op of
    OpEQ -> lua_equal l (fromStackIndex idx1) (fromStackIndex idx2)
    OpLT -> lua_lessthan l (fromStackIndex idx1) (fromStackIndex idx2)
    OpLE -> (+) <$> lua_equal l (fromStackIndex idx1) (fromStackIndex idx2)
                <*> lua_lessthan l (fromStackIndex idx1) (fromStackIndex idx2)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_concat lua_concat>.
concat :: Int -> Lua ()
concat n = liftLua $ \l -> lua_concat l (fromIntegral n)

-- | Copies the element at index @fromidx@ into the valid index @toidx@,
-- replacing the value at that position. Values at other positions are not
-- affected.
--
-- See also <https://www.lua.org/manual/5.3/manual.html#lua_copy lua_copy> in
-- the lua manual.
copy :: StackIndex -> StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
copy fromidx toidx = liftLua $ \l ->
  lua_copy l (fromStackIndex fromidx) (fromStackIndex toidx)
#else
copy fromidx toidx = do
  pushvalue fromidx
  remove toidx
  insert toidx
#endif

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_cpcall lua_cpcall>.
cpcall :: FunPtr LuaCFunction -> Ptr a -> Lua Int
#if LUA_VERSION_NUMBER >= 502
cpcall a c = do
  pushcfunction a
  pushlightuserdata c
  pcall 1 0 0
#else
cpcall a c = liftLua $ \l -> fmap fromIntegral (lua_cpcall l a c)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_createtable lua_createtable>.
createtable :: Int -> Int -> Lua ()
createtable s z = liftLua $ \l ->
  lua_createtable l (fromIntegral s) (fromIntegral z)

-- TODO: implement dump

-- | Tests whether the objects under the given indices are equal. See
-- <https://www.lua.org/manual/5.3/manual.html#lua_equal lua_equal>.
equal :: StackIndex -> StackIndex -> Lua Bool
equal index1 index2 = compare index1 index2 OpEQ

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gc lua_gc>.
gc :: GCCONTROL -> Int -> Lua Int
gc i j= liftLua $ \l ->
  fromIntegral <$> lua_gc l (fromIntegral (fromEnum i)) (fromIntegral j)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getfield lua_getfield>.
getfield :: StackIndex -> String -> Lua ()
getfield i s = liftLua $ \l ->
  withCString s $ \sPtr -> lua_getfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getglobal lua_getglobal>.
getglobal :: String -> Lua ()
#if LUA_VERSION_NUMBER >= 502
getglobal s = liftLua $ \l ->
  withCString s $ \sPtr -> lua_getglobal l sPtr
#else
getglobal s = getfield globalsindex s
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_getmetatable lua_getmetatable>.
getmetatable :: StackIndex -> Lua Bool
getmetatable n = liftLua $ \l ->
  fmap (/= 0) (lua_getmetatable l (fromStackIndex n))

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gettable lua_gettable>.
gettable :: StackIndex -> Lua ()
gettable n = liftLua $ \l -> lua_gettable l (fromStackIndex n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_gettop lua_gettop>.
gettop :: Lua StackIndex
gettop = liftLua $ fmap fromIntegral . lua_gettop

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_insert lua_insert>.
insert :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
insert index = liftLua $ \l -> lua_rotate l (fromIntegral index) 1
#else
insert index = liftLua $ \l -> lua_insert l (fromIntegral index)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isboolean lua_isboolean>.
isboolean :: StackIndex -> Lua Bool
isboolean n = (== TBOOLEAN) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_iscfunction lua_iscfunction>.
iscfunction :: StackIndex -> Lua Bool
iscfunction n = liftLua $ \l -> (/= 0) <$> lua_iscfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isfunction lua_isfunction>.
isfunction :: StackIndex -> Lua Bool
isfunction n = liftM (== TFUNCTION) (ltype n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_islightuserdata lua_islightuserdata>.
islightuserdata :: StackIndex -> Lua Bool
islightuserdata n = (== TLIGHTUSERDATA) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnil lua_isnil>.
isnil :: StackIndex -> Lua Bool
isnil n = (== TNIL) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnone lua_isnone>.
isnone :: StackIndex -> Lua Bool
isnone n = (== TNONE) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnoneornil lua_isnoneornil>.
isnoneornil :: StackIndex -> Lua Bool
isnoneornil idx = (<= TNIL) <$> (ltype idx)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isnumber lua_isnumber>.
isnumber :: StackIndex -> Lua Bool
isnumber n = liftLua $ \l -> (/= 0) <$> lua_isnumber l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isstring lua_isstring>.
isstring :: StackIndex -> Lua Bool
isstring n = liftLua $ \l -> (/= 0) <$> lua_isstring l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_istable lua_istable>.
istable :: StackIndex -> Lua Bool
istable n = (== TTABLE) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isthread lua_isthread>.
isthread :: StackIndex -> Lua Bool
isthread n = (== TTHREAD) <$> ltype n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_isuserdata lua_isuserdata>.
isuserdata :: StackIndex -> Lua Bool
isuserdata n = liftLua $ \l -> (/= 0) <$> lua_isuserdata l (fromIntegral n)

-- | This is a convenience function to implement error propagation convention
-- described in [Error handling in hslua](#g:1). hslua doesn't implement
-- `lua_error` function from Lua C API because it's never safe to use. (see
-- [Error handling in hslua](#g:1) for details)
lerror :: Lua Int
lerror = do
  getglobal "_HASKELLERR"
  insert (-2)
  return 2

-- | Tests whether the object under the first index is smaller than that under
-- the second. See
-- <https://www.lua.org/manual/5.3/manual.html#lua_lessthan lua_lessthan>.
lessthan :: StackIndex -> StackIndex -> Lua Bool
lessthan index1 index2 = compare index1 index2 OpLT

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: String -> Lua Int
#if LUA_VERSION_NUMBER >= 502
loadfile f = liftLua $ \l ->
  withCString f $ \fPtr ->
  fromIntegral <$> luaL_loadfilex l fPtr nullPtr
#else
loadfile f = liftLua $ \l ->
  withCString f $ \fPtr ->
  fromIntegral <$> luaL_loadfile l fPtr
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: String -> Lua Int
loadstring str = liftLua $ \l ->
  withCString str $ \strPtr ->
  fromIntegral <$> luaL_loadstring l strPtr

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_type lua_type>.
ltype :: StackIndex -> Lua LTYPE
ltype idx = toEnum . fromIntegral <$>
  liftLua (flip lua_type $ fromStackIndex idx)

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: String -> Lua Int
newmetatable s = liftLua $ \l ->
  withCString s $ \sPtr -> fromIntegral <$> luaL_newmetatable l sPtr

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO LuaState
newstate = do
    l <- luaL_newstate
    runLuaWith l $ do
      createtable 0 0
      setglobal "_HASKELLERR"
      return l

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newtable lua_newtable>.
newtable :: Lua ()
newtable = createtable 0 0

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_newuserdata lua_newuserdata>.
newuserdata :: Int -> Lua (Ptr ())
newuserdata = liftLua1 lua_newuserdata . fromIntegral

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_next lua_next>.
next :: Int -> Lua Bool
next i = liftLua $ \l -> (/= 0) <$> lua_next l (fromIntegral i)

{-# DEPRECATED objlen "Use rawlen instead." #-}
-- | Obsolete alias for @'rawlen'@.
objlen :: StackIndex -> Lua Int
objlen = rawlen

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_openlibs luaL_openlibs>.
openlibs :: Lua ()
openlibs = liftLua luaL_openlibs

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_base luaopen_base>.
openbase :: Lua ()
openbase = pushcfunction lua_open_base_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_table luaopen_table>.
opentable :: Lua ()
opentable = pushcfunction lua_open_table_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_io luaopen_io>.
openio :: Lua ()
openio = pushcfunction lua_open_io_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_os luaopen_os>.
openos :: Lua ()
openos = pushcfunction lua_open_os_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_string luaopen_string>.
openstring :: Lua ()
openstring = pushcfunction lua_open_string_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_math luaopen_math>.
openmath :: Lua ()
openmath = pushcfunction lua_open_math_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_debug luaopen_debug>.
opendebug :: Lua ()
opendebug = pushcfunction lua_open_debug_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#pdf-luaopen_package luaopen_package>.
openpackage :: Lua ()
openpackage = pushcfunction lua_open_package_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pcall lua_pcall>.
pcall :: NumArgs -> NumResults -> Int -> Lua Int
#if LUA_VERSION_NUMBER >= 502
pcall nargs nresults errfunc = liftLua $ \l ->
  fromIntegral <$>
  lua_pcallk l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
    0
    nullPtr
#else
pcall nargs nresults errfunc = liftLua $ \l ->
  fromIntegral <$>
  lua_pcall l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pop lua_pop>.
pop :: StackIndex -> Lua ()
pop idx = settop (-idx - 1)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushboolean lua_pushboolean>.
pushboolean :: Bool -> Lua ()
pushboolean v = liftLua $ \l -> lua_pushboolean l (fromIntegral (fromEnum v))

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushcclosure lua_pushcclosure>.
pushcclosure :: FunPtr LuaCFunction -> Int -> Lua ()
pushcclosure f n = liftLua $ \l -> lua_pushcclosure l f (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: FunPtr LuaCFunction -> Lua ()
pushcfunction f = pushcclosure f 0

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushinteger lua_pushinteger>.
pushinteger :: LuaInteger -> Lua ()
pushinteger = liftLua1 lua_pushinteger

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushlightuserdata lua_pushlightuserdata>.
pushlightuserdata :: Ptr a -> Lua ()
pushlightuserdata = liftLua1 lua_pushlightuserdata

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnil lua_pushnil>.
pushnil :: Lua ()
pushnil = liftLua lua_pushnil

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushnumber lua_pushnumber>.
pushnumber :: LuaNumber -> Lua ()
pushnumber = liftLua1 lua_pushnumber

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushstring lua_pushstring>.
pushstring :: B.ByteString -> Lua ()
pushstring s = liftLua $ \l ->
  B.unsafeUseAsCStringLen s $ \(sPtr, z) -> lua_pushlstring l sPtr (fromIntegral z)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_pushvalue lua_pushvalue>.
pushvalue :: StackIndex -> Lua ()
pushvalue n = liftLua $ \l -> lua_pushvalue l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawequal lua_rawequal>.
rawequal :: StackIndex -> StackIndex -> Lua Bool
rawequal n m = liftLua $ \l ->
  (/= 0) <$> lua_rawequal l (fromIntegral n) (fromIntegral m)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawget lua_rawget>.
rawget :: StackIndex -> Lua ()
rawget n = liftLua $ \l -> lua_rawget l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: StackIndex -> Int -> Lua ()
rawgeti k m = liftLua $ \l -> lua_rawgeti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawset lua_rawset>.
rawset :: StackIndex -> Lua ()
rawset n = liftLua $ \l -> lua_rawset l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawseti lua_rawseti>.
rawseti :: StackIndex -> Int -> Lua ()
rawseti k m = liftLua $ \l -> lua_rawseti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_rawlen lua_rawlen>.
rawlen :: StackIndex -> Lua Int
#if LUA_VERSION_NUMBER >= 502
rawlen idx = liftLua $ \l -> fromIntegral <$> lua_rawlen l (fromIntegral idx)
#else
rawlen idx = liftLua $ \l -> fromIntegral <$> lua_objlen l (fromIntegral idx)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_ref luaL_ref>.
ref :: StackIndex -> Lua Int
ref t = liftLua $ \l -> fromIntegral <$> luaL_ref l (fromStackIndex t)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_register lua_register>.
register :: String -> FunPtr LuaCFunction -> Lua ()
register n f = do
    pushcclosure f 0
    setglobal n

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_remove lua_remove>.
remove :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
remove n = liftLua (\l -> lua_rotate l (fromIntegral n) (-1)) *> pop 1
#else
remove n = liftLua $ \l -> lua_remove l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_replace lua_replace>.
replace :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
replace n = liftLua (\l -> lua_copy l (-1) (fromIntegral n)) *> pop 1
#else
replace n = liftLua $ \l ->  lua_replace l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setfield lua_setfield>.
setfield :: StackIndex -> String -> Lua ()
setfield i s = liftLua $ \l ->
  withCString s $ \sPtr -> lua_setfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setglobal lua_setglobal>.
setglobal :: String -> Lua ()
#if LUA_VERSION_NUMBER >= 502
setglobal s = liftLua $ \l -> withCString s $ \sPtr -> lua_setglobal l sPtr
#else
setglobal n = setfield globalsindex n
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_setmetatable lua_setmetatable>.
setmetatable :: Int -> Lua ()
setmetatable n = liftLua $ \l -> lua_setmetatable l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_settable lua_settable>.
settable :: Int -> Lua ()
settable index = liftLua $ \l -> lua_settable l (fromIntegral index)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_settop lua_settop>.
settop :: StackIndex -> Lua ()
settop = liftLua1 lua_settop . fromStackIndex

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_status lua_status>.
status :: Lua Int
status = liftLua $ fmap fromIntegral . lua_status

{-# DEPRECATED strlen "Use rawlen instead." #-}
-- | Compatibility alias for rawlen
strlen :: StackIndex -> Lua Int
strlen = rawlen

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_toboolean lua_toboolean>.
toboolean :: StackIndex -> Lua Bool
toboolean n = liftLua $ \l -> (/= 0) <$> lua_toboolean l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: StackIndex -> Lua (FunPtr LuaCFunction)
tocfunction n = liftLua $ \l -> lua_tocfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tointeger lua_tointeger>.
tointeger :: StackIndex -> Lua LuaInteger
#if LUA_VERSION_NUMBER >= 502
tointeger n = liftLua $ \l -> lua_tointegerx l (fromIntegral n) 0
#else
tointeger n = liftLua $ \l -> lua_tointeger l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tonumber lua_tonumber>.
tonumber :: StackIndex -> Lua LuaNumber
#if LUA_VERSION_NUMBER >= 502
tonumber n = liftLua $ \l -> lua_tonumberx l (fromIntegral n) 0
#else
tonumber n = liftLua $ \l -> lua_tonumber l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_topointer lua_topointer>.
topointer :: StackIndex -> Lua (Ptr ())
topointer n = liftLua $ \l -> lua_topointer l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tostring lua_tostring>.
tostring :: StackIndex -> Lua B.ByteString
tostring n = liftLua $ \l -> alloca $ \lenPtr -> do
    cstr <- lua_tolstring l (fromIntegral n) lenPtr
    cstrLen <- F.peek lenPtr
    B.packCStringLen (cstr, fromIntegral cstrLen)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_tothread lua_tothread>.
tothread :: StackIndex -> Lua LuaState
tothread n = liftLua $ \l -> lua_tothread l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_touserdata lua_touserdata>.
touserdata :: StackIndex -> Lua (Ptr a)
touserdata n = liftLua $ \l -> lua_touserdata l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_typename lua_typename>.
typename :: LTYPE -> Lua String
typename n = liftLua $ \l ->
  lua_typename l (fromIntegral (fromEnum n)) >>= peekCString

-- | See <https://www.lua.org/manual/5.3/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -> Int -> Lua ()
unref idx r = liftLua $ \l ->
  luaL_unref l (fromStackIndex idx) (fromIntegral r)

-- | See <https://www.lua.org/manual/5.3/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: StackIndex -> StackIndex
#if LUA_VERSION_NUMBER >= 502
upvalueindex i = registryindex - i
#else
upvalueindex i = globalsindex - i
#endif
