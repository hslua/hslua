{-# LANGUAGE FlexibleInstances, ForeignFunctionInterface, ScopedTypeVariables #-}

-- In older versions of GHC, FlexibleInstances doesn't imply
-- TypeSynonymInstances, so we need to enable it explicitly.
-- See #29.
{-# LANGUAGE TypeSynonymInstances #-}

module Scripting.Lua
  ( LuaState
  , LuaCFunction
  , LuaInteger
  , LuaNumber
  , module Scripting.Lua
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import qualified Foreign.Storable as F
import Prelude hiding (concat)
import qualified Prelude

import Scripting.Lua.Raw

#include "lua.h"

-- | Enumeration used as type tag. See <https://www.lua.org/manual/5.1/manual.html#lua_type lua_type>.
data LTYPE
    = TNONE
    | TNIL
    | TBOOLEAN
    | TLIGHTUSERDATA
    | TNUMBER
    | TSTRING
    | TTABLE
    | TFUNCTION
    | TUSERDATA
    | TTHREAD
    deriving (Eq,Show,Ord)

instance Enum LTYPE where
    fromEnum TNONE          = #{const LUA_TNONE}
    fromEnum TNIL           = #{const LUA_TNIL}
    fromEnum TBOOLEAN       = #{const LUA_TBOOLEAN}
    fromEnum TLIGHTUSERDATA = #{const LUA_TLIGHTUSERDATA}
    fromEnum TNUMBER        = #{const LUA_TNUMBER}
    fromEnum TSTRING        = #{const LUA_TSTRING}
    fromEnum TTABLE         = #{const LUA_TTABLE}
    fromEnum TFUNCTION      = #{const LUA_TFUNCTION}
    fromEnum TUSERDATA      = #{const LUA_TUSERDATA}
    fromEnum TTHREAD        = #{const LUA_TTHREAD}

    toEnum (#{const LUA_TNONE})          = TNONE
    toEnum (#{const LUA_TNIL})           = TNIL
    toEnum (#{const LUA_TBOOLEAN})       = TBOOLEAN
    toEnum (#{const LUA_TLIGHTUSERDATA}) = TLIGHTUSERDATA
    toEnum (#{const LUA_TNUMBER})        = TNUMBER
    toEnum (#{const LUA_TSTRING})        = TSTRING
    toEnum (#{const LUA_TTABLE})         = TTABLE
    toEnum (#{const LUA_TFUNCTION})      = TFUNCTION
    toEnum (#{const LUA_TUSERDATA})      = TUSERDATA
    toEnum (#{const LUA_TTHREAD})        = TTHREAD
    toEnum n                             = error $ "Cannot convert (" ++ show n ++ ") to LTYPE"

-- | Enumeration used by @gc@ function.
data GCCONTROL
    = GCSTOP
    | GCRESTART
    | GCCOLLECT
    | GCCOUNT
    | GCCOUNTB
    | GCSTEP
    | GCSETPAUSE
    | GCSETSTEPMUL
    deriving (Eq,Ord,Show,Enum)

-- | Alias for C constant @LUA_MULTRET@. See <https://www.lua.org/manual/5.1/manual.html#lua_call lua_call>.
multret :: Int
multret = #{const LUA_MULTRET}

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_settop lua_settop>.
settop :: LuaState -> Int -> IO ()
settop l n = c_lua_settop l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_createtable lua_createtable>.
createtable :: LuaState -> Int -> Int -> IO ()
createtable l s z = c_lua_createtable l (fromIntegral s) (fromIntegral z)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_objlen lua_objlen>.
objlen :: LuaState -> Int -> IO Int
objlen l n = liftM fromIntegral (c_lua_objlen l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pop lua_pop>.
pop :: LuaState -> Int -> IO ()
pop l n = settop l (-n-1)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_newtable lua_newtable>.
newtable :: LuaState -> IO ()
newtable l = createtable l 0 0

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushcclosure lua_pushcclosure>.
pushcclosure :: LuaState -> FunPtr LuaCFunction -> Int -> IO ()
pushcclosure l f n = c_lua_pushcclosure l f (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: LuaState -> FunPtr LuaCFunction -> IO ()
pushcfunction l f = pushcclosure l f 0

{-# DEPRECATED strlen "Use objlen instead." #-}
-- | Compatibility alias for objlen
strlen :: LuaState -> Int -> IO Int
strlen = objlen

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_type lua_type>.
ltype :: LuaState -> Int -> IO LTYPE
ltype l n = liftM (toEnum . fromIntegral) (c_lua_type l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isfunction lua_isfunction>.
isfunction :: LuaState -> Int -> IO Bool
isfunction l n = liftM (== TFUNCTION) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_istable lua_istable>.
istable :: LuaState -> Int -> IO Bool
istable l n = liftM (== TTABLE) (ltype l n)

-- | Try to convert Lua array at given index to Haskell list.
tolist :: StackValue a => LuaState -> Int -> IO (Maybe [a])
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

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_islightuserdata lua_islightuserdata>.
islightuserdata :: LuaState -> Int -> IO Bool
islightuserdata l n = liftM (== TLIGHTUSERDATA) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isnil lua_isnil>.
isnil :: LuaState -> Int -> IO Bool
isnil l n = liftM (== TNIL) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isboolean lua_isboolean>.
isboolean :: LuaState -> Int -> IO Bool
isboolean l n = liftM (== TBOOLEAN) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isthread lua_isthread>.
isthread :: LuaState -> Int -> IO Bool
isthread l n = liftM (== TTHREAD) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isnone lua_isnone>.
isnone :: LuaState -> Int -> IO Bool
isnone l n = liftM (== TNONE) (ltype l n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isnoneornil lua_isnoneornil>.
isnoneornil :: LuaState -> Int -> IO Bool
isnoneornil l n = liftM (<= TNIL) (ltype l n)

-- | Alias for C constant @LUA_REGISTRYINDEX@. See <https://www.lua.org/manual/5.1/manual.html#3.5 Lua registry>.
registryindex :: Int
registryindex = #{const LUA_REGISTRYINDEX}

-- | Alias for C constant @LUA_ENVIRONINDEX@. See <https://www.lua.org/manual/5.1/manual.html#3.3 pseudo-indices>.
environindex :: Int
environindex = #{const LUA_ENVIRONINDEX}

-- | Alias for C constant @LUA_GLOBALSINDEX@. See <https://www.lua.org/manual/5.1/manual.html#3.3 pseudo-indices>.
globalsindex :: Int
globalsindex = #{const LUA_GLOBALSINDEX}

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: Int -> Int
upvalueindex i = globalsindex - i

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_atpanic lua_atpanic>.
atpanic :: LuaState -> FunPtr LuaCFunction -> IO (FunPtr LuaCFunction)
atpanic = c_lua_atpanic

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tostring lua_tostring>.
tostring :: LuaState -> Int -> IO B.ByteString
tostring l n = alloca $ \lenPtr -> do
    cstr <- c_lua_tolstring l (fromIntegral n) lenPtr
    cstrLen <- F.peek lenPtr
    B.packCStringLen (cstr, fromIntegral cstrLen)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tothread lua_tothread>.
tothread :: LuaState -> Int -> IO LuaState
tothread l n = c_lua_tothread l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_touserdata lua_touserdata>.
touserdata :: LuaState -> Int -> IO (Ptr a)
touserdata l n = c_lua_touserdata l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_typename lua_typename>.
typename :: LuaState -> LTYPE -> IO String
typename l n = c_lua_typename l (fromIntegral (fromEnum n)) >>= peekCString

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_xmove lua_xmove>.
xmove :: LuaState -> LuaState -> Int -> IO ()
xmove l1 l2 n = c_lua_xmove l1 l2 (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_yield lua_yield>.
yield :: LuaState -> Int -> IO Int
yield l n = liftM fromIntegral (c_lua_yield l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_checkstack lua_checkstack>.
checkstack :: LuaState -> Int -> IO Bool
checkstack l n = liftM (/= 0) (c_lua_checkstack l (fromIntegral n))

-- | See @lua_newstate@ and <https://www.lua.org/manual/5.1/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO LuaState
newstate = do
    l <- c_luaL_newstate
    createtable l 0 0
    setglobal l "_HASKELLERR"
    return l

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_close lua_close>.
close :: LuaState -> IO ()
close = c_lua_close

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_concat lua_concat>.
concat :: LuaState -> Int -> IO ()
concat l n = c_lua_concat l (fromIntegral n)

-- | See @lua_call@ and <https://www.lua.org/manual/5.1/manual.html#lua_call lua_call>.
call :: LuaState -> Int -> Int -> IO ()
call l a b = c_lua_call l (fromIntegral a) (fromIntegral b)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pcall lua_pcall>.
pcall :: LuaState -> Int -> Int -> Int -> IO Int
pcall l a b c = liftM fromIntegral (c_lua_pcall l (fromIntegral a) (fromIntegral b) (fromIntegral c))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_cpcall lua_cpcall>.
cpcall :: LuaState -> FunPtr LuaCFunction -> Ptr a -> IO Int
cpcall l a c = liftM fromIntegral (c_lua_cpcall l a c)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_getfield lua_getfield>.
getfield :: LuaState -> Int -> String -> IO ()
getfield l i s = withCString s $ \sPtr -> c_lua_getfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_setfield lua_setfield>.
setfield :: LuaState -> Int -> String -> IO ()
setfield l i s = withCString s $ \sPtr -> c_lua_setfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_getglobal lua_getglobal>.
getglobal :: LuaState -> String -> IO ()
getglobal l n = getfield l globalsindex n

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_setglobal lua_setglobal>.
setglobal :: LuaState -> String -> IO ()
setglobal l n = setfield l globalsindex n

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_openlibs luaL_openlibs>.
openlibs :: LuaState -> IO ()
openlibs = c_luaL_openlibs

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_base luaopen_base>.
openbase :: LuaState -> IO ()
openbase l = pushcfunction l c_lua_open_base_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_table luaopen_table>.
opentable :: LuaState -> IO ()
opentable l = pushcfunction l c_lua_open_table_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_io luaopen_io>.
openio :: LuaState -> IO ()
openio l = pushcfunction l c_lua_open_io_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_os luaopen_os>.
openos :: LuaState -> IO ()
openos l = pushcfunction l c_lua_open_os_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_string luaopen_string>.
openstring :: LuaState -> IO ()
openstring l = pushcfunction l c_lua_open_string_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_math luaopen_math>.
openmath :: LuaState -> IO ()
openmath l = pushcfunction l c_lua_open_math_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_debug luaopen_debug>.
opendebug :: LuaState -> IO ()
opendebug l = pushcfunction l c_lua_open_debug_ptr >> call l 0 multret

-- | See <https://www.lua.org/manual/5.1/manual.html#pdf-luaopen_package luaopen_package>.
openpackage :: LuaState -> IO ()
openpackage l = pushcfunction l c_lua_open_package_ptr >> call l 0 multret

foreign import ccall "wrapper" mkStringWriter :: LuaWriter -> IO (FunPtr LuaWriter)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_dump lua_dump>.
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

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_equal lua_equal>.
equal :: LuaState -> Int -> Int -> IO Bool
equal l i j = liftM (/= 0) (c_lua_equal l (fromIntegral i) (fromIntegral j))

-- | This is a convenience function to implement error propagation convention
-- described in [Error handling in hslua](#g:1). hslua doesn't implement
-- `lua_error` function from Lua C API because it's never safe to use. (see
-- [Error handling in hslua](#g:1) for details)
lerror :: LuaState -> IO Int
lerror l = do
    getglobal l "_HASKELLERR"
    insert l (-2)
    return 2

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_gc lua_gc>.
gc :: LuaState -> GCCONTROL -> Int -> IO Int
gc l i j= liftM fromIntegral (c_lua_gc l (fromIntegral (fromEnum i)) (fromIntegral j))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_getfenv lua_getfenv>.
getfenv :: LuaState -> Int -> IO ()
getfenv l n = c_lua_getfenv l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_getmetatable lua_getmetatable>.
getmetatable :: LuaState -> Int -> IO Bool
getmetatable l n = liftM (/= 0) (c_lua_getmetatable l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_gettable lua_gettable>.
gettable :: LuaState -> Int -> IO ()
gettable l n = c_lua_gettable l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_gettop lua_gettop>.
gettop :: LuaState -> IO Int
gettop l = liftM fromIntegral (c_lua_gettop l)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_insert lua_insert>.
insert :: LuaState -> Int -> IO ()
insert l n  = c_lua_insert l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_iscfunction lua_iscfunction>.
iscfunction :: LuaState -> Int -> IO Bool
iscfunction l n = liftM (/= 0) (c_lua_iscfunction l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isnumber lua_isnumber>.
isnumber :: LuaState -> Int -> IO Bool
isnumber l n = liftM (/= 0) (c_lua_isnumber l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isstring lua_isstring>.
isstring :: LuaState -> Int -> IO Bool
isstring l n = liftM (/= 0) (c_lua_isstring l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_isuserdata lua_isuserdata>.
isuserdata :: LuaState -> Int -> IO Bool
isuserdata l n = liftM (/= 0) (c_lua_isuserdata l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_lessthan lua_lessthan>.
lessthan :: LuaState -> Int -> Int -> IO Bool
lessthan l i j = liftM (/= 0) (c_lua_lessthan l (fromIntegral i) (fromIntegral j))


-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: LuaState -> String -> IO Int
loadfile l f = withCString f $ \fPtr -> fmap fromIntegral (c_luaL_loadfile l fPtr)

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: LuaState -> String -> IO Int
loadstring l str = withCString str $ \strPtr -> fmap fromIntegral (c_luaL_loadstring l strPtr)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_newthread lua_newthread>.
newthread :: LuaState -> IO LuaState
newthread l = c_lua_newthread l

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_newuserdata lua_newuserdata>.
newuserdata :: LuaState -> Int -> IO (Ptr ())
newuserdata l s = c_lua_newuserdata l (fromIntegral s)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_next lua_next>.
next :: LuaState -> Int -> IO Bool
next l i = liftM (/= 0) (c_lua_next l (fromIntegral i))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushboolean lua_pushboolean>.
pushboolean :: LuaState -> Bool -> IO ()
pushboolean l v = c_lua_pushboolean l (fromIntegral (fromEnum v))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushinteger lua_pushinteger>.
pushinteger :: LuaState -> LuaInteger -> IO ()
pushinteger = c_lua_pushinteger

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushlightuserdata lua_pushlightuserdata>.
pushlightuserdata :: LuaState -> Ptr a -> IO ()
pushlightuserdata = c_lua_pushlightuserdata

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushnil lua_pushnil>.
pushnil :: LuaState -> IO ()
pushnil = c_lua_pushnil

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushnumber lua_pushnumber>.
pushnumber :: LuaState -> LuaNumber -> IO ()
pushnumber = c_lua_pushnumber

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushstring lua_pushstring>.
pushstring :: LuaState -> B.ByteString -> IO ()
pushstring l s = B.unsafeUseAsCStringLen s $ \(sPtr, z) -> c_lua_pushlstring l sPtr (fromIntegral z)

-- | Push a list to Lua stack as a Lua array.
pushlist :: StackValue a => LuaState -> [a] -> IO ()
pushlist l list = do
    newtable l
    forM_ (zip [1..] list) $ \(idx, val) -> do
      push l val
      rawseti l (-2) idx

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushthread lua_pushthread>.
pushthread :: LuaState -> IO Bool
pushthread l = liftM (/= 0) (c_lua_pushthread l)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_pushvalue lua_pushvalue>.
pushvalue :: LuaState -> Int -> IO ()
pushvalue l n = c_lua_pushvalue l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_rawequal lua_rawequal>.
rawequal :: LuaState -> Int -> Int -> IO Bool
rawequal l n m = liftM (/= 0) (c_lua_rawequal l (fromIntegral n) (fromIntegral m))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_rawget lua_rawget>.
rawget :: LuaState -> Int -> IO ()
rawget l n = c_lua_rawget l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: LuaState -> Int -> Int -> IO ()
rawgeti l k m = c_lua_rawgeti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_rawset lua_rawset>.
rawset :: LuaState -> Int -> IO ()
rawset l n = c_lua_rawset l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_rawseti lua_rawseti>.
rawseti :: LuaState -> Int -> Int -> IO ()
rawseti l k m = c_lua_rawseti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_remove lua_remove>.
remove :: LuaState -> Int -> IO ()
remove l n = c_lua_remove l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_replace lua_replace>.
replace :: LuaState -> Int -> IO ()
replace l n = c_lua_replace l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_resume lua_resume>.
resume :: LuaState -> Int -> IO Int
resume l n = liftM fromIntegral (c_lua_resume l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_setfenv lua_setfenv>.
setfenv :: LuaState -> Int -> IO Int
setfenv l n = liftM fromIntegral (c_lua_setfenv l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_setmetatable lua_setmetatable>.
setmetatable :: LuaState -> Int -> IO ()
setmetatable l n = c_lua_setmetatable l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_settable lua_settable>.
settable :: LuaState -> Int -> IO ()
settable l index = c_lua_settable l (fromIntegral index)


-- | See <https://www.lua.org/manual/5.1/manual.html#lua_status lua_status>.
status :: LuaState -> IO Int
status l = liftM fromIntegral (c_lua_status l)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_toboolean lua_toboolean>.
toboolean :: LuaState -> Int -> IO Bool
toboolean l n = liftM (/= 0) (c_lua_toboolean l (fromIntegral n))

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: LuaState -> Int -> IO (FunPtr LuaCFunction)
tocfunction l n = c_lua_tocfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tointeger lua_tointeger>.
tointeger :: LuaState -> Int -> IO LuaInteger
tointeger l n = c_lua_tointeger l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_tonumber lua_tonumber>.
tonumber :: LuaState -> Int -> IO LuaNumber
tonumber l n = c_lua_tonumber l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_topointer lua_topointer>.
topointer :: LuaState -> Int -> IO (Ptr ())
topointer l n = c_lua_topointer l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#lua_register lua_register>.
register :: LuaState -> String -> FunPtr LuaCFunction -> IO ()
register l n f = do
    pushcclosure l f 0
    setglobal l n

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: LuaState -> String -> IO Int
newmetatable l s = withCString s $ \sPtr -> liftM fromIntegral (c_luaL_newmetatable l sPtr)

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_argerror luaL_argerror>. Contrary to the
-- manual, Haskell function does return with value less than zero.
argerror :: LuaState -> Int -> String -> IO CInt
argerror l n msg = withCString msg $ \msgPtr -> do
    let doit l' = c_luaL_argerror l' (fromIntegral n) msgPtr
    f <- mkWrapper doit
    _ <- c_lua_cpcall l f nullPtr
    freeHaskellFunPtr f
    -- here we should have error message string on top of the stack
    return (-1)

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_ref luaL_ref>.
ref :: LuaState -> Int -> IO Int
ref l n = fmap fromIntegral $ c_luaL_ref l (fromIntegral n)

-- | See <https://www.lua.org/manual/5.1/manual.html#luaL_unref luaL_unref>.
unref :: LuaState -> Int -> Int -> IO ()
unref l t r = c_luaL_unref l (fromIntegral t) (fromIntegral r)

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
    peek :: LuaState -> Int -> IO (Maybe a)
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


typenameindex :: LuaState -> Int -> IO String
typenameindex l n = ltype l n >>= typename l

class LuaImport a where
    luaimport' :: Int -> a -> LuaCFunction
    luaimportargerror :: Int -> String -> a -> LuaCFunction

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
            (Prelude.concat ["argument ", show narg, " of Haskell function: ",
                             expected, " expected, got ", got])
            (x undefined) l

foreign import ccall "wrapper" mkWrapper :: LuaCFunction -> IO (FunPtr LuaCFunction)

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
    callproc' :: LuaState -> String -> IO () -> Int -> a

-- | Call a Lua procedure. Use as:
--
-- > callproc l "proc" "abc" (1::Int) (5.0::Double)
--
callproc :: (LuaCallProc a) => LuaState -> String -> a
callproc l f = callproc' l f (return ()) 0

class LuaCallFunc a where
    callfunc' :: LuaState -> String -> IO () -> Int -> a

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
