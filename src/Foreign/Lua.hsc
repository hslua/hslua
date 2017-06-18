{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Lua
  ( module Foreign.Lua
  , module Foreign.Lua.Constants
  , module Foreign.Lua.Types
  ) where

import Prelude hiding ( compare, concat )

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Foreign.C
import Foreign.Lua.Bindings
import Foreign.Lua.Constants
import Foreign.Lua.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import qualified Foreign.Storable as F
import qualified Prelude

#include "lua.h"

-- | Turn a function of typ @LuaState -> IO a@ into a monadic lua operation.
liftLua :: (LuaState -> IO a) -> Lua a
liftLua f = luaState >>= liftIO . f

-- | Turn a function of typ @LuaState -> a -> IO b@ into a monadic lua operation.
liftLua1 :: (LuaState -> a -> IO b) -> a -> Lua b
liftLua1 f x = liftLua $ \l -> f l x

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_settop lua_settop>.
settop :: StackIndex -> Lua ()
settop = liftLua1 c_lua_settop . fromStackIndex

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_createtable lua_createtable>.
createtable :: Int -> Int -> Lua ()
createtable s z = liftLua $ \l ->
  c_lua_createtable l (fromIntegral s) (fromIntegral z)

--
-- Size of objects
--

-- | See
-- <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawlen\
-- lua_rawlen>.
rawlen :: StackIndex -> Lua Int
#if LUA_VERSION_NUMBER >= 502
rawlen idx = liftLua $ \l -> fromIntegral <$> c_lua_rawlen l (fromIntegral idx)
#else
rawlen idx = liftLua $ \l -> fromIntegral <$> c_lua_objlen l (fromIntegral idx)
#endif

{-# DEPRECATED objlen "Use rawlen instead." #-}
-- | Obso
objlen :: StackIndex -> Lua Int
objlen = rawlen

{-# DEPRECATED strlen "Use rawlen instead." #-}
-- | Compatibility alias for objlen
strlen :: StackIndex -> Lua Int
strlen = objlen


--
-- Get Functions
--

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pop lua_pop>.
pop :: StackIndex -> Lua ()
pop idx = settop (-idx - 1)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newtable lua_newtable>.
newtable :: Lua ()
newtable = createtable 0 0


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushcclosure lua_pushcclosure>.
pushcclosure :: FunPtr LuaCFunction -> Int -> Lua ()
pushcclosure f n = liftLua $ \l -> c_lua_pushcclosure l f (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushcfunction lua_pushcfunction>.
pushcfunction :: FunPtr LuaCFunction -> Lua ()
pushcfunction f = pushcclosure f 0

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_type lua_type>.
ltype :: StackIndex -> Lua LTYPE
ltype idx = toEnum . fromIntegral <$>
  liftLua (flip c_lua_type $ fromStackIndex idx)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isfunction lua_isfunction>.
isfunction :: StackIndex -> Lua Bool
isfunction n = liftM (== TFUNCTION) (ltype n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_istable lua_istable>.
istable :: StackIndex -> Lua Bool
istable n = (== TTABLE) <$> ltype n

-- | Try to convert Lua array at given index to Haskell list.
tolist :: StackValue a => StackIndex -> Lua (Maybe [a])
tolist n = do
  len <- objlen n
  iter [1..len]
 where
  iter [] = return $ Just []
  iter (i : is) = do
    rawgeti n i
    ret <- peek (-1)
    pop 1
    case ret of
      Nothing  -> return Nothing
      Just val -> do
        rest <- iter is
        return $ case rest of
                   Nothing -> Nothing
                   Just vals -> Just (val : vals)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_islightuserdata lua_islightuserdata>.
islightuserdata :: StackIndex -> Lua Bool
islightuserdata n = (== TLIGHTUSERDATA) <$> ltype n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnil lua_isnil>.
isnil :: StackIndex -> Lua Bool
isnil n = (== TNIL) <$> ltype n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isboolean lua_isboolean>.
isboolean :: StackIndex -> Lua Bool
isboolean n = (== TBOOLEAN) <$> ltype n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isthread lua_isthread>.
isthread :: StackIndex -> Lua Bool
isthread n = (== TTHREAD) <$> ltype n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnone lua_isnone>.
isnone :: StackIndex -> Lua Bool
isnone n = (== TNONE) <$> ltype n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnoneornil lua_isnoneornil>.
isnoneornil :: StackIndex -> Lua Bool
isnoneornil idx = (<= TNIL) <$> (ltype idx)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_upvalueindex lua_upvalueindex>.
upvalueindex :: StackIndex -> StackIndex
#if LUA_VERSION_NUMBER >= 502
upvalueindex i = registryindex - i
#else
upvalueindex i = globalsindex - i
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_atpanic lua_atpanic>.
atpanic :: FunPtr LuaCFunction -> Lua (FunPtr LuaCFunction)
atpanic = liftLua1 c_lua_atpanic

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tostring lua_tostring>.
tostring :: StackIndex -> Lua B.ByteString
tostring n = liftLua $ \l -> alloca $ \lenPtr -> do
    cstr <- c_lua_tolstring l (fromIntegral n) lenPtr
    cstrLen <- F.peek lenPtr
    B.packCStringLen (cstr, fromIntegral cstrLen)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tothread lua_tothread>.
tothread :: StackIndex -> Lua LuaState
tothread n = liftLua $ \l -> c_lua_tothread l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_touserdata lua_touserdata>.
touserdata :: StackIndex -> Lua (Ptr a)
touserdata n = liftLua $ \l -> c_lua_touserdata l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_typename lua_typename>.
typename :: LTYPE -> Lua String
typename n = liftLua $ \l ->
  c_lua_typename l (fromIntegral (fromEnum n)) >>= peekCString

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_xmove lua_xmove>.
xmove :: LuaState -> Int -> Lua ()
xmove l2 n = liftLua $ \l1 -> c_lua_xmove l1 l2 (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_yield lua_yield>.
yield :: Int -> Lua Int
#if LUA_VERSION_NUMBER >= 502
yield n = liftLua $ \l -> fromIntegral <$> c_lua_yieldk l (fromIntegral n) 0 nullPtr
#else
yield n = liftLua $ \l -> fromIntegral <$> (c_lua_yield l (fromIntegral n))
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_checkstack lua_checkstack>.
checkstack :: Int -> Lua Bool
checkstack n = liftLua $ \l -> liftM (/= 0) (c_lua_checkstack l (fromIntegral n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_newstate luaL_newstate>.
newstate :: IO LuaState
newstate = do
    l <- c_luaL_newstate
    runLuaWith l $ do
      createtable 0 0
      setglobal "_HASKELLERR"
      return l

-- | Run lua computation using the default HsLua state as starting point.
runLua :: Lua a -> IO a
runLua lua = do
  st <- newstate
  res <- runLuaWith st lua
  c_lua_close st
  return res

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_close lua_close>.
close :: LuaState -> IO ()
close = c_lua_close

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_concat lua_concat>.
concat :: Int -> Lua ()
concat n = liftLua $ \l -> c_lua_concat l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_call lua_call>.
call :: NumArgs -> NumResults -> Lua ()
#if LUA_VERSION_NUMBER >= 502
call a nresults = liftLua $ \l ->
  c_lua_callk l (fromNumArgs a) (fromNumResults nresults) 0 nullPtr
#else
call a nresults = liftLua $ \l ->
  c_lua_call l (fromNumArgs a) (fromNumResults nresults)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pcall lua_pcall>.
pcall :: NumArgs -> NumResults -> Int -> Lua Int
#if LUA_VERSION_NUMBER >= 502
pcall nargs nresults errfunc = liftLua $ \l ->
  fromIntegral <$>
  c_lua_pcallk l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
    0
    nullPtr
#else
pcall nargs nresults errfunc = liftLua $ \l ->
  fromIntegral <$>
  c_lua_pcall l
    (fromNumArgs nargs)
    (fromNumResults nresults)
    (fromIntegral errfunc)
#endif

-- | See <https://www.lua.org/manual/5.2/manual.html#lua_cpcall lua_cpcall>.
cpcall :: FunPtr LuaCFunction -> Ptr a -> Lua Int
#if LUA_VERSION_NUMBER >= 502
cpcall a c = do
  pushcfunction a
  pushlightuserdata c
  pcall 1 0 0
#else
cpcall a c = liftLua $ \l -> fmap fromIntegral (c_lua_cpcall l a c)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getfield lua_getfield>.
getfield :: StackIndex -> String -> Lua ()
getfield i s = liftLua $ \l ->
  withCString s $ \sPtr -> c_lua_getfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setfield lua_setfield>.
setfield :: StackIndex -> String -> Lua ()
setfield i s = liftLua $ \l ->
  withCString s $ \sPtr -> c_lua_setfield l (fromIntegral i) sPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getglobal lua_getglobal>.
getglobal :: String -> Lua ()
#if LUA_VERSION_NUMBER >= 502
getglobal s = liftLua $ \l ->
  withCString s $ \sPtr -> c_lua_getglobal l sPtr
#else
getglobal s = getfield globalsindex s
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setglobal lua_setglobal>.
setglobal :: String -> Lua ()
#if LUA_VERSION_NUMBER >= 502
setglobal s = liftLua $ \l -> withCString s $ \sPtr -> c_lua_setglobal l sPtr
#else
setglobal n = setfield globalsindex n
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_openlibs luaL_openlibs>.
openlibs :: Lua ()
openlibs = liftLua c_luaL_openlibs

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_base luaopen_base>.
openbase :: Lua ()
openbase = pushcfunction c_lua_open_base_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_table luaopen_table>.
opentable :: Lua ()
opentable = pushcfunction c_lua_open_table_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_io luaopen_io>.
openio :: Lua ()
openio = pushcfunction c_lua_open_io_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_os luaopen_os>.
openos :: Lua ()
openos = pushcfunction c_lua_open_os_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_string luaopen_string>.
openstring :: Lua ()
openstring = pushcfunction c_lua_open_string_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_math luaopen_math>.
openmath :: Lua ()
openmath = pushcfunction c_lua_open_math_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_debug luaopen_debug>.
opendebug :: Lua ()
opendebug = pushcfunction c_lua_open_debug_ptr *> call 0 multret

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#pdf-luaopen_package luaopen_package>.
openpackage :: Lua ()
openpackage = pushcfunction c_lua_open_package_ptr *> call 0 multret

foreign import ccall "wrapper" mkStringWriter :: LuaWriter -> IO (FunPtr LuaWriter)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_dump lua_dump>.
dump :: Lua String
dump = liftLua $ \l -> do
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

compare :: StackIndex -> StackIndex -> LuaComparerOp -> Lua Bool
#if LUA_VERSION_NUMBER >= 502
compare idx1 idx2 op = liftLua $ \l -> (/= 0) <$>
  c_lua_compare l
    (fromStackIndex idx1)
    (fromStackIndex idx2)
    (fromIntegral (fromEnum op))
#else
compare idx1 idx2 op = liftLua $ \l ->
  (/= 0) <$>
  case op of
    OpEQ -> c_lua_equal l (fromStackIndex idx1) (fromStackIndex idx2)
    OpLT -> c_lua_lessthan l (fromStackIndex idx1) (fromStackIndex idx2)
    OpLE -> (+) <$> c_lua_equal l (fromStackIndex idx1) (fromStackIndex idx2)
                <*> c_lua_lessthan l (fromStackIndex idx1) (fromStackIndex idx2)
#endif

-- | Tests whether the objects under the given indices are equal. See
-- <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_equal
-- lua_equal>.
equal :: StackIndex -> StackIndex -> Lua Bool
equal index1 index2 = compare index1 index2 OpEQ

-- | Tests whether the object under the first index is smaller than that under
-- the second. See
-- <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_lessthan
-- lua_lessthan>.
lessthan :: StackIndex -> StackIndex -> Lua Bool
lessthan index1 index2 = compare index1 index2 OpLT

-- | This is a convenience function to implement error propagation convention
-- described in [Error handling in hslua](#g:1). hslua doesn't implement
-- `lua_error` function from Lua C API because it's never safe to use. (see
-- [Error handling in hslua](#g:1) for details)
lerror :: Lua Int
lerror = do
  getglobal "_HASKELLERR"
  insert (-2)
  return 2

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gc lua_gc>.
gc :: GCCONTROL -> Int -> Lua Int
gc i j= liftLua $ \l ->
  fromIntegral <$> c_lua_gc l (fromIntegral (fromEnum i)) (fromIntegral j)

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getfenv lua_getfenv>.
getfenv :: Int -> Lua ()
getfenv n = liftLua $ \l ->
  c_lua_getfenv l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_getmetatable lua_getmetatable>.
getmetatable :: StackIndex -> Lua Bool
getmetatable n = liftLua $ \l ->
  fmap (/= 0) (c_lua_getmetatable l (fromStackIndex n))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gettable lua_gettable>.
gettable :: StackIndex -> Lua ()
gettable n = liftLua $ \l -> c_lua_gettable l (fromStackIndex n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_gettop lua_gettop>.
gettop :: Lua StackIndex
gettop = liftLua $ fmap fromIntegral . c_lua_gettop

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_insert lua_insert>.
insert :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
insert index = liftLua $ \l -> c_lua_rotate l (fromIntegral index) 1
#else
insert index = liftLua $ \l -> c_lua_insert l (fromIntegral index)
#endif

#if LUA_VERSION_NUMBER >= 503
copy :: StackIndex -> StackIndex -> Lua ()
copy fromidx toidx = liftLua $ \l ->
  c_lua_copy l (fromStackIndex fromidx) (fromStackIndex toidx)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_iscfunction lua_iscfunction>.
iscfunction :: StackIndex -> Lua Bool
iscfunction n = liftLua $ \l -> (/= 0) <$> c_lua_iscfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isnumber lua_isnumber>.
isnumber :: StackIndex -> Lua Bool
isnumber n = liftLua $ \l -> (/= 0) <$> c_lua_isnumber l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isstring lua_isstring>.
isstring :: StackIndex -> Lua Bool
isstring n = liftLua $ \l -> (/= 0) <$> c_lua_isstring l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_isuserdata lua_isuserdata>.
isuserdata :: StackIndex -> Lua Bool
isuserdata n = liftLua $ \l -> (/= 0) <$> c_lua_isuserdata l (fromIntegral n)


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_loadfile luaL_loadfile>.
loadfile :: String -> Lua Int
#if LUA_VERSION_NUMBER >= 502
loadfile f = liftLua $ \l ->
  withCString f $ \fPtr ->
  fromIntegral <$> c_luaL_loadfilex l fPtr nullPtr
#else
loadfile f = liftLua $ \l ->
  withCString f $ \fPtr ->
  fromIntegral <$> c_luaL_loadfile l fPtr
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_loadstring luaL_loadstring>.
loadstring :: String -> Lua Int
loadstring str = liftLua $ \l ->
  withCString str $ \strPtr ->
  fromIntegral <$> c_luaL_loadstring l strPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newthread lua_newthread>.
newthread :: Lua LuaState
newthread = liftLua c_lua_newthread

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_newuserdata lua_newuserdata>.
newuserdata :: Int -> Lua (Ptr ())
newuserdata = liftLua1 c_lua_newuserdata . fromIntegral

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_next lua_next>.
next :: Int -> Lua Bool
next i = liftLua $ \l -> (/= 0) <$> c_lua_next l (fromIntegral i)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushboolean lua_pushboolean>.
pushboolean :: Bool -> Lua ()
pushboolean v = liftLua $ \l -> c_lua_pushboolean l (fromIntegral (fromEnum v))

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushinteger lua_pushinteger>.
pushinteger :: LuaInteger -> Lua ()
pushinteger = liftLua1 c_lua_pushinteger

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushlightuserdata lua_pushlightuserdata>.
pushlightuserdata :: Ptr a -> Lua ()
pushlightuserdata = liftLua1 c_lua_pushlightuserdata

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushnil lua_pushnil>.
pushnil :: Lua ()
pushnil = liftLua c_lua_pushnil

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushnumber lua_pushnumber>.
pushnumber :: LuaNumber -> Lua ()
pushnumber = liftLua1 c_lua_pushnumber

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushstring lua_pushstring>.
pushstring :: B.ByteString -> Lua ()
pushstring s = liftLua $ \l ->
  B.unsafeUseAsCStringLen s $ \(sPtr, z) -> c_lua_pushlstring l sPtr (fromIntegral z)

-- | Push a list to Lua stack as a Lua array.
pushlist :: StackValue a => [a] -> Lua ()
pushlist list = do
  newtable
  forM_ (zip [1..] list) $ \(idx, val) -> do
    push val
    rawseti (-2) idx

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushthread lua_pushthread>.
pushthread :: Lua Bool
pushthread = (/= 0) <$> liftLua c_lua_pushthread

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_pushvalue lua_pushvalue>.
pushvalue :: StackIndex -> Lua ()
pushvalue n = liftLua $ \l -> c_lua_pushvalue l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawequal lua_rawequal>.
rawequal :: StackIndex -> StackIndex -> Lua Bool
rawequal n m = liftLua $ \l ->
  (/= 0) <$> c_lua_rawequal l (fromIntegral n) (fromIntegral m)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawget lua_rawget>.
rawget :: StackIndex -> Lua ()
rawget n = liftLua $ \l -> c_lua_rawget l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawgeti lua_rawgeti>.
rawgeti :: StackIndex -> Int -> Lua ()
rawgeti k m = liftLua $ \l -> c_lua_rawgeti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawset lua_rawset>.
rawset :: StackIndex -> Lua ()
rawset n = liftLua $ \l -> c_lua_rawset l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_rawseti lua_rawseti>.
rawseti :: StackIndex -> Int -> Lua ()
rawseti k m = liftLua $ \l -> c_lua_rawseti l (fromIntegral k) (fromIntegral m)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_remove lua_remove>.
remove :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
remove n = liftLua (\l -> c_lua_rotate l (fromIntegral n) (-1)) *> pop 1
#else
remove n = liftLua $ \l -> c_lua_remove l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_replace lua_replace>.
replace :: StackIndex -> Lua ()
#if LUA_VERSION_NUMBER >= 503
replace n = liftLua (\l -> c_lua_copy l (-1) (fromIntegral n)) *> pop 1
#else
replace n = liftLua $ \l ->  c_lua_replace l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_resume lua_resume>.
resume :: Int -> Lua Int
resume n = liftLua $ \l -> fromIntegral <$> c_lua_resume l (fromIntegral n)

#if LUA_VERSION_NUMBER < 502
-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setfenv lua_setfenv>.
setfenv :: Int -> Lua Int
setfenv n = liftLua $ \l -> fromIntegral <$> c_lua_setfenv l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_setmetatable lua_setmetatable>.
setmetatable :: Int -> Lua ()
setmetatable n = liftLua $ \l -> c_lua_setmetatable l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_settable lua_settable>.
settable :: Int -> Lua ()
settable index = liftLua $ \l -> c_lua_settable l (fromIntegral index)


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_status lua_status>.
status :: Lua Int
status = liftLua $ fmap fromIntegral . c_lua_status


--
-- Type coercion
--

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_toboolean lua_toboolean>.
toboolean :: StackIndex -> Lua Bool
toboolean n = liftLua $ \l -> (/= 0) <$> c_lua_toboolean l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tocfunction lua_tocfunction>.
tocfunction :: StackIndex -> Lua (FunPtr LuaCFunction)
tocfunction n = liftLua $ \l -> c_lua_tocfunction l (fromIntegral n)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tointeger lua_tointeger>.
tointeger :: StackIndex -> Lua LuaInteger
#if LUA_VERSION_NUMBER >= 502
tointeger n = liftLua $ \l -> c_lua_tointegerx l (fromIntegral n) 0
#else
tointeger n = liftLua $ \l -> c_lua_tointeger l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_tonumber lua_tonumber>.
tonumber :: StackIndex -> Lua LuaNumber
#if LUA_VERSION_NUMBER >= 502
tonumber n = liftLua $ \l -> c_lua_tonumberx l (fromIntegral n) 0
#else
tonumber n = liftLua $ \l -> c_lua_tonumber l (fromIntegral n)
#endif

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_topointer lua_topointer>.
topointer :: StackIndex -> Lua (Ptr ())
topointer n = liftLua $ \l -> c_lua_topointer l (fromIntegral n)


-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#lua_register lua_register>.
register :: String -> FunPtr LuaCFunction -> Lua ()
register n f = do
    pushcclosure f 0
    setglobal n

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_newmetatable luaL_newmetatable>.
newmetatable :: String -> Lua Int
newmetatable s = liftLua $ \l ->
  withCString s $ \sPtr -> fromIntegral <$> c_luaL_newmetatable l sPtr

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_argerror luaL_argerror>. Contrary to the
-- manual, Haskell function does return with value less than zero.
argerror :: Int -> String -> Lua CInt
argerror n msg = do
  f <- liftIO $ withCString msg $ \msgPtr -> do
    let doit l' = c_luaL_argerror l' (fromIntegral n) msgPtr
    mkWrapper doit
  _ <- cpcall f nullPtr
  liftIO $ freeHaskellFunPtr f
  -- here we should have error message string on top of the stack
  return (-1)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_ref luaL_ref>.
ref :: StackIndex -> Lua Int
ref t = liftLua $ \l -> fromIntegral <$> c_luaL_ref l (fromStackIndex t)

-- | See <https://www.lua.org/manual/LUA_VERSION_MAJORMINOR/manual.html#luaL_unref luaL_unref>.
unref :: StackIndex -> Int -> Lua ()
unref idx r = liftLua $ \l ->
  c_luaL_unref l (fromStackIndex idx) (fromIntegral r)

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
    push :: a -> Lua ()
    -- | Check if at index @n@ there is a convertible Lua value and if so return it
    -- wrapped in @Just@. Return @Nothing@ otherwise.
    peek :: StackIndex -> Lua (Maybe a)
    -- | Lua type id code of the vaule expected. Parameter is unused.
    valuetype :: a -> LTYPE

maybepeek :: n -> (n -> Lua Bool) -> (n -> Lua r) -> Lua (Maybe r)
maybepeek n test peekfn = do
    v <- test n
    if v
      then Just <$> (peekfn n)
      else return Nothing

instance StackValue LuaInteger where
    push x = pushinteger x
    peek n = maybepeek n isnumber tointeger
    valuetype _ = TNUMBER

instance StackValue LuaNumber where
    push x = pushnumber x
    peek n = maybepeek n isnumber tonumber
    valuetype _ = TNUMBER

instance StackValue Int where
    push x = pushinteger (fromIntegral x)
    peek n = maybepeek n isnumber (fmap fromIntegral . tointeger)
    valuetype _ = TNUMBER

instance StackValue B.ByteString where
    push x = pushstring x
    peek n = maybepeek n isstring tostring
    valuetype _ = TSTRING

instance StackValue a => StackValue [a] where
    push x = pushlist x
    peek n = tolist n
    valuetype _ = TTABLE

instance StackValue Bool where
    push x = pushboolean x
    peek n = maybepeek n isboolean toboolean
    valuetype _ = TBOOLEAN

instance StackValue (FunPtr LuaCFunction) where
    push x = pushcfunction x
    peek n = maybepeek n iscfunction tocfunction
    valuetype _ = TFUNCTION

-- watch out for the asymetry here
instance StackValue (Ptr a) where
    push x = pushlightuserdata x
    peek n = maybepeek n isuserdata touserdata
    valuetype _ = TUSERDATA

-- watch out for push here
instance StackValue LuaState where
    push _ = pushthread >> return ()
    peek n = maybepeek n isthread tothread
    valuetype _ = TTHREAD

instance StackValue () where
    push _ = pushnil
    peek n = maybepeek n isnil . const $ return ()
    valuetype _ = TNIL

-- | Like @getglobal@, but knows about packages. e. g.
--
-- > getglobal l "math.sin"
--
-- returns correct result
getglobal2 :: String -> Lua ()
getglobal2 n = do
    getglobal x
    mapM_ dotable xs
  where
    (x : xs)  = splitdot n
    splitdot  = filter (/= ".") . L.groupBy (\a b -> a /= '.' && b /= '.')
    dotable a = getfield (-1) a *> gettop >>= \i -> remove (i - 1)


typenameindex :: StackIndex -> Lua String
typenameindex n = ltype n >>= typename

class LuaImport a where
  luaimport' :: StackIndex -> a -> Lua CInt
  luaimportargerror :: StackIndex -> String -> a -> Lua CInt

instance (StackValue a) => LuaImport (IO a) where
  luaimportargerror n msg x = luaimportargerror n msg (liftIO x :: Lua a)
  luaimport' narg x = luaimport' narg (liftIO x :: Lua a)

instance (StackValue a) => LuaImport (LuaState -> IO a) where
  luaimportargerror n msg = luaimportargerror n msg . liftLua
  luaimport' narg = luaimport' narg . liftLua

instance (StackValue a) => LuaImport (Lua a) where
  luaimportargerror _n msg _x = do
    -- TODO: maybe improve the error message
    pushstring (BC.pack msg)
    fromIntegral <$> lerror
  luaimport' _narg x = (x >>= push) *> return 1

instance (StackValue a, LuaImport b) => LuaImport (a -> b) where
  luaimportargerror n msg x = luaimportargerror n msg (x undefined)
  luaimport' narg x = do
    arg <- peek narg
    case arg of
      Just v -> luaimport' (narg+1) (x v)
      Nothing -> do
        t <- ltype narg
        expected <- typename $ valuetype (fromJust arg)
        got <- typename t
        luaimportargerror narg
          (Prelude.concat [ "argument ", show (fromStackIndex narg)
                          , " of Haskell function: ", expected
                          , " expected, got ", got])
          (x undefined)

foreign import ccall "wrapper" mkWrapper :: LuaCFunction -> IO (FunPtr LuaCFunction)

-- | Create new foreign Lua function. Function created can be called
-- by Lua engine. Remeber to free the pointer with @freecfunction@.
newcfunction :: LuaImport a => a -> IO (FunPtr LuaCFunction)
newcfunction = mkWrapper . flip runLuaWith . luaimport

-- | Convert a Haskell function to Lua function. Any Haskell function
-- can be converted provided that:
--
--   * all arguments are instances of StackValue
--   * return type is IO t, where t is an instance of StackValue
--
-- Any Haskell exception will be converted to a string and returned
-- as Lua error.
luaimport :: LuaImport a => a -> Lua CInt
luaimport a = luaimport' 1 a

-- | Free function pointer created with @newcfunction@.
freecfunction :: FunPtr LuaCFunction -> IO ()
freecfunction = freeHaskellFunPtr

class LuaCallProc a where
  callproc' :: String -> Lua () -> NumArgs -> a

-- | Call a Lua procedure. Use as:
--
-- > callproc "proc" "abc" (1::Int) (5.0::Double)
--
callproc :: (LuaCallProc a) => String -> a
callproc f = callproc' f (return ()) 0

class LuaCallFunc a where
    callfunc' :: String -> Lua () -> NumArgs -> a

-- | Call a Lua function. Use as:
--
-- > Just v <- callfunc l "proc" "abc" (1::Int) (5.0::Double)
callfunc :: (LuaCallFunc a) => String -> a
callfunc f = callfunc' f (return ()) 0

instance LuaCallProc (Lua t) where
  callproc' f a k = do
    getglobal2 f
    a
    z <- pcall k 0 0
    if z /= 0
      then do
        Just msg <- peek (-1)
        pop 1
        fail (BC.unpack msg)
      else return undefined

instance (StackValue t) => LuaCallFunc (Lua t) where
  callfunc' f a k = do
    getglobal2 f
    a
    z <- pcall k 1 0
    if z /= 0
      then do
        Just msg <- peek (-1)
        pop 1
        fail (BC.unpack msg)
      else do
        r <- peek (-1)
        pop 1
        case r of
          Just x -> return x
          Nothing -> do
            expected <- typename (valuetype (fromJust r))
            t <- ltype (-1)
            got <- typename t
            fail $ Prelude.concat
              [ "Incorrect result type (", expected, " expected, got ", got, ")" ]

instance (StackValue t, LuaCallProc b) => LuaCallProc (t -> b) where
    callproc' f a k x = callproc' f (a *> push x) (k+1)

instance (StackValue t, LuaCallFunc b) => LuaCallFunc (t -> b) where
    callfunc' f a k x = callfunc' f (a *> push x) (k+1)


foreign export ccall hsmethod__gc :: LuaState -> IO CInt
foreign import ccall "&hsmethod__gc" hsmethod__gc_addr :: FunPtr LuaCFunction

foreign export ccall hsmethod__call :: LuaState -> IO CInt
foreign import ccall "&hsmethod__call" hsmethod__call_addr :: FunPtr LuaCFunction

hsmethod__gc :: LuaState -> IO CInt
hsmethod__gc l = do
  Just ptr <- runLuaWith l $ peek (-1)
  stableptr <- F.peek (castPtr ptr)
  freeStablePtr stableptr
  return 0

hsmethod__call :: LuaState -> IO CInt
hsmethod__call l = do
  Just ptr <- runLuaWith l $ peek 1 <* remove 1
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
pushhsfunction :: LuaImport a => a -> Lua ()
pushhsfunction = pushrawhsfunction . flip runLuaWith . luaimport

-- | Pushes _raw_ Haskell function converted to a Lua function.
-- Raw Haskell functions collect parameters from the stack and return
-- a `CInt` that represents number of return values left in the stack.
pushrawhsfunction :: LuaCFunction -> Lua ()
pushrawhsfunction f = do
  stableptr <- liftIO $ newStablePtr f
  p <- newuserdata (F.sizeOf stableptr)
  liftIO $ F.poke (castPtr p) stableptr
  v <- newmetatable "HaskellImportedFunction"
  when (v /= 0) $ do
    -- create new metatable, fill it with two entries __gc and __call
    push hsmethod__gc_addr
    setfield (-2) "__gc"
    push hsmethod__call_addr
    setfield (-2) "__call"
  setmetatable (-2)
  return ()

-- | Imports a Haskell function and registers it at global name.
registerhsfunction :: LuaImport a => String -> a -> Lua ()
registerhsfunction n f = pushhsfunction f *> setglobal n

-- | Imports a raw Haskell function and registers it at global name.
registerrawhsfunction :: String -> LuaCFunction -> Lua ()
registerrawhsfunction n f = pushrawhsfunction f *> setglobal n


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
