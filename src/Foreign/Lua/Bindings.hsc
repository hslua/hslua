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
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Foreign.Lua.Bindings
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017 Albert Krewinkel,
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Haskell-friendly wrappers of C bindings.
-}
module Foreign.Lua.Bindings where

import Prelude hiding ( compare, concat )

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ( (<$>) )
#endif
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import Foreign.C
import Foreign.Lua.Raw
import Foreign.Lua.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Foreign.Storable as F

#include "lua.h"

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_settop lua_settop>.
settop :: LuaState -> StackIndex -> IO ()
settop l idx = c_lua_settop l (fromStackIndex idx)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_createtable lua_createtable>.
createtable :: LuaState -> Int -> Int -> IO ()
createtable l s z = c_lua_createtable l (fromIntegral s) (fromIntegral z)

--
-- Size of objects
--

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_objlen lua_objlen>.
objlen :: LuaState -> StackIndex -> IO Int
#if LUA_VERSION_NUMBER >= 502
objlen = rawlen

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_objlen lua_objlen>.
rawlen :: LuaState -> StackIndex -> IO Int
rawlen l n = liftM fromIntegral (c_lua_rawlen l (fromIntegral n))
#else
objlen l n = liftM fromIntegral (c_lua_objlen l (fromIntegral n))
#endif


--
-- Get Functions
--

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pop lua_pop>.
pop :: LuaState -> StackIndex -> IO ()
pop l idx = settop l (-idx - 1)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newtable lua_newtable>.
newtable :: LuaState -> IO ()
newtable l = createtable l 0 0

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushcclosure lua_pushcclosure>.
pushcclosure :: LuaState -> FunPtr LuaCFunction -> Int -> IO ()
pushcclosure l f n = c_lua_pushcclosure l f (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: LuaState -> FunPtr LuaCFunction -> IO ()
pushcfunction l f = pushcclosure l f 0

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_type lua_type>.
ltype :: LuaState -> StackIndex -> IO LTYPE
ltype l n = liftM (toEnum . fromIntegral) (c_lua_type l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isfunction lua_isfunction>.
isfunction :: LuaState -> StackIndex -> IO Bool
isfunction l n = liftM (== TFUNCTION) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_istable lua_istable>.
istable :: LuaState -> StackIndex -> IO Bool
istable l n = liftM (== TTABLE) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_islightuserdata lua_islightuserdata>.
islightuserdata :: LuaState -> StackIndex -> IO Bool
islightuserdata l n = liftM (== TLIGHTUSERDATA) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnil lua_isnil>.
isnil :: LuaState -> StackIndex -> IO Bool
isnil l n = liftM (== TNIL) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isboolean lua_isboolean>.
isboolean :: LuaState -> StackIndex -> IO Bool
isboolean l n = liftM (== TBOOLEAN) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isthread lua_isthread>.
isthread :: LuaState -> StackIndex -> IO Bool
isthread l n = liftM (== TTHREAD) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnone lua_isnone>.
isnone :: LuaState -> StackIndex -> IO Bool
isnone l n = liftM (== TNONE) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnoneornil lua_isnoneornil>.
isnoneornil :: LuaState -> StackIndex -> IO Bool
isnoneornil l n = liftM (<= TNIL) (ltype l n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_atpanic lua_atpanic>.
atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)
atpanic = c_lua_atpanic

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tostring lua_tostring>.
tostring :: LuaState -> StackIndex -> IO B.ByteString
tostring l n = alloca $ \lenPtr -> do
    cstr <- c_lua_tolstring l (fromIntegral n) lenPtr
    cstrLen <- F.peek lenPtr
    B.packCStringLen (cstr, fromIntegral cstrLen)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tothread lua_tothread>.
tothread :: LuaState -> StackIndex -> IO LuaState
tothread l n = c_lua_tothread l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_touserdata lua_touserdata>.
touserdata :: LuaState -> StackIndex -> IO (Ptr a)
touserdata l n = c_lua_touserdata l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_typename lua_typename>.
typename :: LuaState -> LTYPE -> IO String
typename l n = c_lua_typename l (fromIntegral (fromEnum n)) >>= peekCString

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_xmove lua_xmove>.
xmove :: LuaState -> LuaState -> Int -> IO ()
xmove l1 l2 n = c_lua_xmove l1 l2 (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_yield lua_yield>.
yield :: LuaState -> Int -> IO Int
#if LUA_VERSION_NUMBER >= 502
yield l n = fromIntegral <$> c_lua_yieldk l (fromIntegral n) 0 nullPtr
#else
yield l n = liftM fromIntegral (c_lua_yield l (fromIntegral n))
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_checkstack lua_checkstack>.
checkstack :: LuaState -> Int -> IO Bool
checkstack l n = liftM (/= 0) (c_lua_checkstack l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO LuaState
newstate = do
    l <- c_luaL_newstate
    createtable l 0 0
    setglobal l "_HASKELLERR"
    return l

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_close lua_close>.
close :: LuaState -> IO ()
close = c_lua_close

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_concat lua_concat>.
concat :: LuaState -> Int -> IO ()
concat l n = c_lua_concat l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_call lua_call>.
call :: LuaState -> NumArgs -> NumResults -> IO ()
#if LUA_VERSION_NUMBER >= 502
call l a nresults =
  c_lua_callk l (fromNumArgs a) (fromNumResults nresults) 0 nullPtr
#else
call l a nresults =
  c_lua_call l (fromNumArgs a) (fromNumResults nresults)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pcall lua_pcall>.
pcall :: LuaState -> NumArgs -> NumResults -> Int -> IO Int
#if LUA_VERSION_NUMBER >= 502
pcall l nargs nresults errfunc = fromIntegral <$>
  c_lua_pcallk l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
    0
    nullPtr
#else
pcall l nargs nresults errfunc = fromIntegral <$>
  c_lua_pcall l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
#endif

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_cpcall lua_cpcall>.
cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO Int
#if LUA_VERSION_NUMBER >= 502
cpcall l a c = do
  pushcfunction l a
  pushlightuserdata l c
  pcall l 1 0 0
#else
cpcall l a c = liftM fromIntegral (c_lua_cpcall l a c)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getfield lua_getfield>.
getfield :: LuaState -> StackIndex -> String -> IO ()
getfield l i s = withCString s $ \sPtr -> c_lua_getfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setfield lua_setfield>.
setfield :: LuaState -> StackIndex -> String -> IO ()
setfield l i s = withCString s $ \sPtr -> c_lua_setfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getglobal lua_getglobal>.
getglobal :: LuaState -> String -> IO ()
#if LUA_VERSION_NUMBER >= 502
getglobal l s = withCString s $ \sPtr -> c_lua_getglobal l sPtr
#else
getglobal l s = getfield l globalsindex s
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setglobal lua_setglobal>.
setglobal :: LuaState -> String -> IO ()
#if LUA_VERSION_NUMBER >= 502
setglobal l s = withCString s $ \sPtr -> c_lua_setglobal l sPtr
#else
setglobal l n = setfield l globalsindex n
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_openlibs luaL_openlibs>.
openlibs :: LuaState -> IO ()
openlibs = c_luaL_openlibs

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_base luaopen_base>.
openbase :: LuaState -> IO ()
openbase l = pushcfunction l c_lua_open_base_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_table luaopen_table>.
opentable :: LuaState -> IO ()
opentable l = pushcfunction l c_lua_open_table_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_io luaopen_io>.
openio :: LuaState -> IO ()
openio l = pushcfunction l c_lua_open_io_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_os luaopen_os>.
openos :: LuaState -> IO ()
openos l = pushcfunction l c_lua_open_os_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_string luaopen_string>.
openstring :: LuaState -> IO ()
openstring l = pushcfunction l c_lua_open_string_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_math luaopen_math>.
openmath :: LuaState -> IO ()
openmath l = pushcfunction l c_lua_open_math_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_debug luaopen_debug>.
opendebug :: LuaState -> IO ()
opendebug l = pushcfunction l c_lua_open_debug_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_package luaopen_package>.
openpackage :: LuaState -> IO ()
openpackage l = pushcfunction l c_lua_open_package_ptr >> call l 0 multret

foreign import ccall "wrapper" mkStringWriter :: LuaWriter -> IO (FunPtr LuaWriter)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_dump lua_dump>.
dump :: LuaState -> IO String
dump l = do
    r <- newIORef ""
    let wr :: LuaWriter
        wr _l p s _d = do
          k <- peekCStringLen (p, fromIntegral s)
          modifyIORef r (++ k)
          return 0
    writer <- mkStringWriter wr
    c_lua_dump l writer nullPtr
    freeHaskellFunPtr writer
    readIORef r

#if LUA_VERSION_NUMBER >= 502
compare :: LuaState -> StackIndex -> StackIndex -> LuaComparerOp -> IO Bool
compare l index1 index2 op = (/= 0) <$>
  c_lua_compare l
    (fromStackIndex index1)
    (fromStackIndex index2)
    (fromIntegral (fromEnum op))

-- | Tests whether the objects under the given indices are equal.
equal :: LuaState -> StackIndex -> StackIndex -> IO Bool
equal l index1 index2 = compare l index1 index2 OpEQ

-- | Tests whether the object under the first index is smaller than that under
-- the second.
lessthan :: LuaState -> StackIndex -> StackIndex -> IO Bool
lessthan l index1 index2 = compare l index1 index2 OpLT

#else

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_equal lua_equal>.
equal :: LuaState -> StackIndex -> StackIndex -> IO Bool
equal l i j = liftM (/= 0) (c_lua_equal l (fromStackIndex i) (fromStackIndex j))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_lessthan lua_lessthan>.
lessthan :: LuaState -> StackIndex -> StackIndex -> IO Bool
lessthan l i j = liftM (/= 0) (c_lua_lessthan l (fromIntegral i) (fromIntegral j))
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gc lua_gc>.
gc :: LuaState -> GCCONTROL -> Int -> IO Int
gc l i j= liftM fromIntegral (c_lua_gc l (fromIntegral (fromEnum i)) (fromIntegral j))

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getfenv lua_getfenv>.
getfenv :: LuaState -> Int -> IO ()
getfenv l n = c_lua_getfenv l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getmetatable lua_getmetatable>.
getmetatable :: LuaState -> StackIndex -> IO Bool
getmetatable l n = liftM (/= 0) (c_lua_getmetatable l (fromStackIndex n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gettable lua_gettable>.
gettable :: LuaState -> StackIndex -> IO ()
gettable l n = c_lua_gettable l (fromStackIndex n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gettop lua_gettop>.
gettop :: LuaState -> IO StackIndex
gettop l = liftM fromIntegral (c_lua_gettop l)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_insert lua_insert>.
insert :: LuaState -> StackIndex -> IO ()
#if LUA_VERSION_NUMBER >= 503
insert l index = c_lua_rotate l (fromIntegral index) 1
#else
insert l index = c_lua_insert l (fromIntegral index)
#endif

#if LUA_VERSION_NUMBER >= 503
copy :: LuaState -> StackIndex -> StackIndex -> IO ()
copy l fromidx toidx = c_lua_copy l (fromStackIndex fromidx) (fromStackIndex toidx)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_iscfunction lua_iscfunction>.
iscfunction :: LuaState -> StackIndex -> IO Bool
iscfunction l n = liftM (/= 0) (c_lua_iscfunction l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnumber lua_isnumber>.
isnumber :: LuaState -> StackIndex -> IO Bool
isnumber l n = liftM (/= 0) (c_lua_isnumber l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isstring lua_isstring>.
isstring :: LuaState -> StackIndex -> IO Bool
isstring l n = liftM (/= 0) (c_lua_isstring l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isuserdata lua_isuserdata>.
isuserdata :: LuaState -> StackIndex -> IO Bool
isuserdata l n = liftM (/= 0) (c_lua_isuserdata l (fromIntegral n))


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: LuaState -> String -> IO Int
#if LUA_VERSION_NUMBER >= 502
loadfile l f = withCString f $ \fPtr ->
  fromIntegral <$> c_luaL_loadfilex l fPtr nullPtr
#else
loadfile l f = withCString f $ \fPtr ->
  fromIntegral <$> c_luaL_loadfile l fPtr
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: LuaState -> String -> IO Int
loadstring l str = withCString str $ \strPtr -> fmap fromIntegral (c_luaL_loadstring l strPtr)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newthread lua_newthread>.
newthread :: LuaState -> IO LuaState
newthread l = c_lua_newthread l

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newuserdata lua_newuserdata>.
newuserdata :: LuaState -> Int -> IO (Ptr ())
newuserdata l s = c_lua_newuserdata l (fromIntegral s)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_next lua_next>.
next :: LuaState -> Int -> IO Bool
next l i = liftM (/= 0) (c_lua_next l (fromIntegral i))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushboolean lua_pushboolean>.
pushboolean :: LuaState -> Bool -> IO ()
pushboolean l v = c_lua_pushboolean l (fromIntegral (fromEnum v))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushinteger lua_pushinteger>.
pushinteger :: LuaState -> LuaInteger -> IO ()
pushinteger = c_lua_pushinteger

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushlightuserdata lua_pushlightuserdata>.
pushlightuserdata :: LuaState -> Ptr a -> IO ()
pushlightuserdata = c_lua_pushlightuserdata

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushnil lua_pushnil>.
pushnil :: LuaState -> IO ()
pushnil = c_lua_pushnil

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushnumber lua_pushnumber>.
pushnumber :: LuaState -> LuaNumber -> IO ()
pushnumber = c_lua_pushnumber

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushstring lua_pushstring>.
pushstring :: LuaState -> B.ByteString -> IO ()
pushstring l s = B.unsafeUseAsCStringLen s $ \(sPtr, z) -> c_lua_pushlstring l sPtr (fromIntegral z)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushthread lua_pushthread>.
pushthread :: LuaState -> IO Bool
pushthread l = liftM (/= 0) (c_lua_pushthread l)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushvalue lua_pushvalue>.
pushvalue :: LuaState -> StackIndex -> IO ()
pushvalue l n = c_lua_pushvalue l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawequal lua_rawequal>.
rawequal :: LuaState -> StackIndex -> StackIndex -> IO Bool
rawequal l n m = liftM (/= 0) (c_lua_rawequal l (fromIntegral n) (fromIntegral m))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawget lua_rawget>.
rawget :: LuaState -> StackIndex -> IO ()
rawget l n = c_lua_rawget l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: LuaState -> StackIndex -> Int -> IO ()
rawgeti l k m = c_lua_rawgeti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawset lua_rawset>.
rawset :: LuaState -> StackIndex -> IO ()
rawset l n = c_lua_rawset l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawseti lua_rawseti>.
rawseti :: LuaState -> StackIndex -> Int -> IO ()
rawseti l k m = c_lua_rawseti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_remove lua_remove>.
remove :: LuaState -> StackIndex -> IO ()
#if LUA_VERSION_NUMBER >= 503
remove l n = c_lua_rotate l (fromIntegral n) (-1) >> pop l 1
#else
remove l n = c_lua_remove l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_replace lua_replace>.
replace :: LuaState -> StackIndex -> IO ()
#if LUA_VERSION_NUMBER >= 503
replace l n = c_lua_copy l (-1) (fromIntegral n) >> pop l 1
#else
replace l n = c_lua_replace l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_resume lua_resume>.
resume :: LuaState -> Int -> IO Int
resume l n = liftM fromIntegral (c_lua_resume l (fromIntegral n))

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setfenv lua_setfenv>.
setfenv :: LuaState -> Int -> IO Int
setfenv l n = liftM fromIntegral (c_lua_setfenv l (fromIntegral n))
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setmetatable lua_setmetatable>.
setmetatable :: LuaState -> Int -> IO ()
setmetatable l n = c_lua_setmetatable l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_settable lua_settable>.
settable :: LuaState -> Int -> IO ()
settable l index = c_lua_settable l (fromIntegral index)


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_status lua_status>.
status :: LuaState -> IO Int
status l = liftM fromIntegral (c_lua_status l)


--
-- Type coercion
--

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_toboolean lua_toboolean>.
toboolean :: LuaState -> StackIndex -> IO Bool
toboolean l n = liftM (/= 0) (c_lua_toboolean l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: LuaState -> StackIndex -> IO (FunPtr LuaCFunction)
tocfunction l n = c_lua_tocfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tointeger lua_tointeger>.
tointeger :: LuaState -> StackIndex -> IO LuaInteger
#if LUA_VERSION_NUMBER >= 502
tointeger l n = c_lua_tointegerx l (fromIntegral n) 0
#else
tointeger l n = c_lua_tointeger l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tonumber lua_tonumber>.
tonumber :: LuaState -> StackIndex -> IO LuaNumber
#if LUA_VERSION_NUMBER >= 502
tonumber l n = c_lua_tonumberx l (fromIntegral n) 0
#else
tonumber l n = c_lua_tonumber l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_topointer lua_topointer>.
topointer :: LuaState -> StackIndex -> IO (Ptr ())
topointer l n = c_lua_topointer l (fromIntegral n)


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_register lua_register>.
register :: LuaState -> String -> FunPtr LuaCFunction -> IO ()
register l n f = do
    pushcclosure l f 0
    setglobal l n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: LuaState -> String -> IO Int
newmetatable l s = withCString s $ \sPtr -> liftM fromIntegral (c_luaL_newmetatable l sPtr)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_argerror luaL_argerror>. Contrary to the
-- manual, Haskell function does return with value less than zero.
argerror :: LuaState -> Int -> String -> IO CInt
argerror l n msg = withCString msg $ \msgPtr -> do
    let doit l' = c_luaL_argerror l' (fromIntegral n) msgPtr
    f <- mkWrapper doit
    _ <- cpcall l f nullPtr
    freeHaskellFunPtr f
    -- here we should have error message string on top of the stack
    return (-1)

foreign import ccall "wrapper" mkWrapper :: LuaCFunction -> IO (FunPtr LuaCFunction)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_ref luaL_ref>.
ref :: LuaState -> StackIndex -> IO Int
ref l t = fromIntegral <$> c_luaL_ref l (fromStackIndex t)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_unref luaL_unref>.
unref :: LuaState -> StackIndex -> Int -> IO ()
unref l idx r = c_luaL_unref l (fromStackIndex idx) (fromIntegral r)
