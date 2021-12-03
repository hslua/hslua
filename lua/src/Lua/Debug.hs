{-|
Module      : Lua.Debug
Copyright   : © 2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Lua debugging utilities.
-}
module Lua.Debug
  ( Debug (..)
  , lua_getstack
  , lua_getinfo
  )
where

import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CInt (..), CUChar (..))
import Foreign.Ptr
import Lua.Types as Lua

-- | A structure used to carry different pieces of information about a
-- function or an activation record.
-- <https://www.lua.org/manual/5.3/manual.html#lua_getstack lua_getstack>
-- fills only the private part of this structure, for later use. To fill
-- the other fields of
-- <https://www.lua.org/manual/5.3/manual.html#lua_Debug lua_Debug> with
-- useful information, call
-- <https://www.lua.org/manual/5.3/manual.html#lua_getinfo lua_getinfo>.
--
-- The fields of
-- <https://www.lua.org/manual/5.3/manual.html#lua_Debug lua_Debug> have
-- the following meaning:
--
-- -   __@source@:__ the name of the chunk that created the function. If
--     @source@ starts with a \'@\@@\', it means that the function was
--     @source@ starts with a \'@=@\', the remainder of its contents
--     describe the source in a user-dependent manner. Otherwise, the
--     function was defined in a string where @source@ is that string.
-- -   __@short_src@:__ a \"printable\" version of @source@, to be used
--     in error messages.
-- -   __@linedefined@:__ the line number where the definition of the
--     function starts.
-- -   __@lastlinedefined@:__ the line number where the definition of the
--     function ends.
-- -   __@what@:__ the string @\"Lua\"@ if the function is a Lua
--     function, @\"C\"@ if it is a C function, @\"main\"@ if it is the
--     main part of a chunk.
-- -   __@currentline@:__ the current line where the given function is
--     executing. When no line information is available, @currentline@ is
--     set to -1.
-- -   __@name@:__ a reasonable name for the given function. Because
--     functions in Lua are first-class values, they do not have a fixed
--     name: some functions can be the value of multiple global
--     variables, while others can be stored only in a table field. The
--     @lua_getinfo@ function checks how the function was called to find
--     a suitable name. If it cannot find a name, then @name@ is set to
--     @NULL@.
-- -   __@namewhat@:__ explains the @name@ field. The value of @namewhat@
--     can be @\"global\"@, @\"local\"@, @\"method\"@, @\"field\"@,
--     @\"upvalue\"@, or @\"\"@ (the empty string), according to how the
--     function was called. (Lua uses the empty string when no other
--     option seems to apply.)
-- -   __@istailcall@:__ true if this function invocation was called by a
--     tail call. In this case, the caller of this level is not in the
--     stack.
-- -   __@nups@:__ the number of upvalues of the function.
-- -   __@nparams@:__ the number of fixed parameters of the function
--     (always 0 for C functions).
-- -   __@isvararg@:__ true if the function is a vararg function (always
--     true for C functions).
data Debug = Debug
  { event           :: CInt     -- ^ (n)
  , name            :: CString  -- ^ (n)
  , namewhat        :: CString  -- ^ (S)
  , what            :: CString  -- ^ (S)
  , source          :: CString  -- ^ (S)
  , currentline     :: CInt     -- ^ (l)
  , linedefined     :: CInt     -- ^ (S)
  , lastlinedefined :: CInt     -- ^ (S)
  , nups            :: CUChar   -- ^ (u) number of upvalues
  , nparams         :: CUChar   -- ^ (u) number of parameters
  , isvararg        :: CChar    -- ^ (u)
  , istailcall      :: CChar    -- ^ (t)
  -- , short_src -- ^ (S)
  }

-- | Gets information about the interpreter runtime stack.
--
-- This function fills parts of a 'Debug' structure with an
-- identification of the /activation record/ of the function executing
-- at a given level. Level 0 is the current running function, whereas
-- level /n+1/ is the function that has called level /n/ (except for
-- tail calls, which do not count on the stack). When there are no
-- errors, @'lua_getstack'@ returns 1; when called with a level greater
-- than the stack depth, it returns 0.
foreign import capi unsafe "lua.h lua_getstack"
  lua_getstack :: Lua.State
               -> CInt       -- ^ level
               -> Ptr Debug
               -> IO CInt

-- | Gets information about a specific function or function invocation.
--
-- To get information about a function invocation, the parameter @ar@ must
-- be a valid activation record that was filled by a previous call to
-- <https://www.lua.org/manual/5.3/manual.html#lua_getstack lua_getstack>
-- or given as argument to a hook (see
-- <https://www.lua.org/manual/5.3/manual.html#lua_Hook lua_Hook>).
--
-- To get information about a function, you push it onto the stack and
-- start the @what@ string with the character \'@>@\'. (In that case,
-- @lua_getinfo@ pops the function from the top of the stack.) For
-- instance, to know in which line a function @f@ was defined, you can
-- write the following code:
--
-- >      lua_Debug ar;
-- >      lua_getglobal(L, "f");  /* get global 'f' */
-- >      lua_getinfo(L, ">S", &ar);
-- >      printf("%d\n", ar.linedefined);
--
-- Each character in the string @what@ selects some fields of the
-- structure @ar@ to be filled or a value to be pushed on the stack:
--
-- -   __\'@n@\':__ fills in the field @name@ and @namewhat@;
--
-- -   __\'@S@\':__ fills in the fields @source@, @short_src@,
--     @linedefined@, @lastlinedefined@, and @what@;
--
-- -   __\'@l@\':__ fills in the field @currentline@;
--
-- -   __\'@t@\':__ fills in the field @istailcall@;
--
-- -   __\'@u@\':__ fills in the fields @nups@, @nparams@, and
--     @isvararg@;
--
-- -   __\'@f@\':__ pushes onto the stack the function that is running at
--     the given level;
--
-- -   __\'@L@\':__ pushes onto the stack a table whose indices are the
--     numbers of the lines that are valid on the function. (A /valid
--     line/ is a line with some associated code, that is, a line where
--     you can put a break point. Non-valid lines include empty lines
--     and comments.)
--
--     If this option is given together with option \'@f@\', its table
--     is pushed after the function.
--
-- This function returns 0 on error (for instance, an invalid option in
-- @what@).
foreign import capi unsafe "lua.h lua_getinfo"
  lua_getinfo :: Lua.State
              -> CString
              -> Ptr Debug
              -> IO CInt
