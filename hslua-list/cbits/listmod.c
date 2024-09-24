#include <stdlib.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#define LIST_T "List"

/* compatibility with older Lua versions, which did not define this in the
 * header. */
#ifndef LUA_LOADED_TABLE
/* key, in the registry, for table of loaded modules */
#define LUA_LOADED_TABLE	"_LOADED"
#endif

/*
** Placeholder function.
*/
static int missing (lua_State *L) {
  return luaL_error(L,
    "Function should have been overwritten with one from the table module."
  );
}

/* Translate a relative table position: negative means back from end */
static lua_Integer posrelat (lua_Integer pos, size_t len) {
  if (pos >= 0) return pos;
  else if (0u - (size_t)pos > len) return 0;
  else return (lua_Integer)len + pos + 1;
}

/*
** Check that 'arg' is either a function or a different callable object.
*/
static void checkcallable (lua_State *L, int arg) {
  if (lua_type(L, arg) != LUA_TFUNCTION) { /* is it not a function? */
    if (luaL_getmetafield(L, arg, "__call"))
      lua_pop(L, 1); /* pop metamethod */
    else
      luaL_checktype(L, arg, LUA_TFUNCTION); /* force an error */
  }
}

/*
** Creates a List from a table; uses a fresh, empty table if none is
** given.
*/
static int list_new (lua_State *L) {
  if (lua_isnoneornil(L, 2)) {
    lua_settop(L, 1);
    lua_newtable(L);
  } else if (lua_type(L, 2) == LUA_TFUNCTION) {
    /* try to use the function as an iterator */
    lua_settop(L, 5);
    lua_newtable(L);     /* create new table */
    /* move the table and toclose variable out of the way */
    lua_insert(L, 2);
    lua_insert(L, 3);
    lua_toclose(L, 3);
    lua_Integer i = 0;   /* list index */
    do {
      lua_pushvalue(L, 4);  /* iterator function */
      lua_pushvalue(L, 5);  /* state */
      lua_rotate(L, 6, -1); /* move control variable to the top */
      lua_call(L, 2, 1);    /* get next list element */
      /* add return value to table */
      lua_pushvalue(L, -1);
      lua_rawseti(L, 2, ++i);
    } while (lua_type(L, -1) != LUA_TNIL);
    lua_settop(L, 2);      /* keep only the new table */
  } else {
    luaL_checktype(L, 2, LUA_TTABLE);
    lua_settop(L, 2);
  }
  lua_pushvalue(L, 1);
  lua_setmetatable(L, 2);
  return 1;
}

/*
** Returns the item at that index, or the default value if no item was
** found.
*/
static int list_at (lua_State *L) {
  lua_settop(L, 3); /* table; index; default value */
  lua_Integer i = luaL_checkinteger(L, 2);
  lua_Integer len = luaL_len(L, 1);

  if (i < -len || i > len) {
    /* out of bounds, do not try to get a value */
    return 1;        /* return the default value */
  }

  i = i >= 0 ? i : len + i + 1;
  if (lua_rawgeti(L, 1, i) == LUA_TNIL) {
    lua_pop(L, 1);  /* pop result; default value is now at the top */
  };
  return 1;
}

/*
** Creates a shallow clone of the given list; the clone will contain
** only the list elements, not any other elements that might have been
** present.
*/
static int list_clone (lua_State *L) {
  lua_settop(L, 1);
  luaL_checktype(L, 1, LUA_TTABLE);
  lua_Integer len = luaL_len(L, 1);
  lua_createtable(L, len, 0);  /* create new table */
  lua_getmetatable(L, 1);
  lua_setmetatable(L, 2);
  for (lua_Integer i = 1; i <= len; i++) {
    lua_geti(L, 1, i);
    lua_seti(L, 2, i);
  }
  return 1;
}

/*
** Creates a new list that is the concatenation of its two arguments.
** The result has the same metatable as the first operand.
*/
static int list_concat (lua_State *L) {
  lua_settop(L, 2);
  luaL_checktype(L, 1, LUA_TTABLE);
  luaL_checktype(L, 2, LUA_TTABLE);
  lua_Integer len1 = luaL_len(L, 1);
  lua_Integer len2 = luaL_len(L, 2);
  lua_createtable(L, len1 + len2, 0);  /* result table */
  if (lua_getmetatable(L, 1)) {
    lua_setmetatable(L, 3);
  }
  for (lua_Integer i = 1; i <= len1; i++) {
    lua_geti(L, 1, i);
    lua_seti(L, 3, i);
  }
  for (lua_Integer i = 1; i <= len2; i++) {
    lua_geti(L, 2, i);
    lua_seti(L, 3, len1 + i);
  }
  return 1;
}

/*
** Checks equality. Two lists are equal if and only if they have the same
** metatable and if all items are equal.
*/
static int list_eq (lua_State *L) {
  lua_settop(L, 2);
  /* compare meta tables */
  if (!(lua_getmetatable(L, 1) &&
        lua_getmetatable(L, 2) &&
        lua_rawequal(L, -1, -2))) {
    lua_pushboolean(L, 0);
    return 1;
  };
  lua_pop(L, 2);  /* remove metatables */

  /* ensure both lists have the same length */
  lua_Integer len1 = luaL_len(L, 1);
  lua_Integer len2 = luaL_len(L, 2);
  if (len1 != len2) {
    lua_pushboolean(L, 0);
    return 1;
  }

  /* check element-wise equality  */
  for (lua_Integer i = 1; i <= len1; i++) {
    lua_geti(L, 1, i);
    lua_geti(L, 2, i);
    if (!lua_compare(L, -1, -2, LUA_OPEQ)) {
      lua_pushboolean(L, 0);
      return 1;
    }
  }
  lua_pushboolean(L, 1);
  return 1;
}

/*
** Appends the second list to the first.
*/
static int list_extend (lua_State *L) {
  lua_settop(L, 2);
  luaL_checktype(L, 1, LUA_TTABLE);
  luaL_checktype(L, 2, LUA_TTABLE);
  lua_Integer len1 = luaL_len(L, 1);
  lua_Integer len2 = luaL_len(L, 2);
  for (lua_Integer i = 1; i <= len2; i++) {
    lua_geti(L, 2, i);
    lua_seti(L, 1, len1 + i);
  }
  return 1;
}

/*
** Removes elements that do not have the desired property.
*/
static int list_filter (lua_State *L) {
  lua_settop(L, 2);
  luaL_checktype(L, 1, LUA_TTABLE);
  checkcallable(L, 2);
  luaL_checkstack(L, 4, NULL);
  lua_Integer len = luaL_len(L, 1);
  lua_createtable(L, len, 0);  /* create new table */
  lua_getmetatable(L, 1) || luaL_getmetatable(L, LIST_T); /* ensure mt */
  lua_setmetatable(L, 3);
  for (lua_Integer i = 1, j = 0; i <= len; i++) {
    lua_pushvalue(L, 2);  /* push predicate function */
    lua_geti(L, 1, i);
    lua_pushinteger(L, i);
    lua_call(L, 2, 1);
    if (lua_toboolean(L, -1)) {
      lua_geti(L, 1, i);
      lua_seti(L, 3, ++j);
    }
    lua_pop(L, 1);  /* remove predicate call result */
  }
  return 1;
}

/*
** Returns the first element that is equal to `needle`, along with that
** element's index, or `nil` if no such element exists.
*/
static int list_find (lua_State *L) {
  luaL_checkstack(L, 2, "List.find");
  lua_settop(L, 3);
  luaL_checktype(L, 1, LUA_TTABLE);
  lua_Integer len = luaL_len(L, 1);
  lua_Integer start = posrelat(luaL_optinteger(L, 3, 1), len);
  for (lua_Integer i = start; i <= len; i++) {
    lua_geti(L, 1, i);
    if (lua_compare(L, 2, -1, LUA_OPEQ)) {
      lua_pushinteger(L, i);
      return 2;
    }
    lua_pop(L, 1);  /* remove list element result */
  }
  lua_pushnil(L);
  return 1;
}

/*
** Returns the first element after the given start index for which the
** predicate function returns a truthy value, along with that element's
** index; returns `nil` if no such element exists.
*/
static int list_find_if (lua_State *L) {
  lua_settop(L, 3);
  luaL_checktype(L, 1, LUA_TTABLE);
  checkcallable(L, 2);
  lua_Integer len = luaL_len(L, 1);
  lua_Integer start = posrelat(luaL_optinteger(L, 3, 1), len);
  for (lua_Integer i = start; i <= len; i++) {
    lua_pushvalue(L, 2);  /* predicate function */
    lua_geti(L, 1, i);
    lua_pushinteger(L, i);
    lua_call(L, 2, 1);
    if (lua_toboolean(L, -1)) {
      lua_geti(L, 1, i);
      lua_pushinteger(L, i);
      return 2;
    }
    lua_pop(L, 1);  /* remove predicate call result */
  }
  lua_pushnil(L);
  return 1;
}

/*
** Returns a boolean value indicating whether or not the element exists
** in the given list.
*/
static int list_includes(lua_State *L) {
  lua_settop(L, 3);
  lua_pushcfunction(L, list_find);
  lua_insert(L, 1);
  lua_call(L, 3, 1);
  luaL_checkstack(L, 1, "List.includes");
  lua_pushboolean(L, lua_toboolean(L, -1));
  return 1;
}

/*
** Returns a copy of the current list by applying the given function to
** all elements.
*/
static int list_map(lua_State *L) {
  lua_settop(L, 2);
  luaL_checktype(L, 1, LUA_TTABLE);
  checkcallable(L, 2);
  lua_Integer len = luaL_len(L, 1);
  lua_createtable(L, len, 0);  /* create new table */
  luaL_getmetatable(L, LIST_T);  /* make result a generic list */
  lua_setmetatable(L, 3);
  for (lua_Integer i = 1; i <= len; i++) {
    lua_pushvalue(L, 2);  /* map function */
    lua_geti(L, 1, i);
    lua_pushinteger(L, i);
    lua_call(L, 2, 1);
    lua_seti(L, 3, i);
  }
  return 1;
}


static void addfield (lua_State *L, luaL_Buffer *b, lua_Integer i) {
  lua_geti(L, 1, i);
  luaL_tolstring(L, -1, NULL);
  lua_remove(L, -2);  /* element */
  luaL_addvalue(b);
}

/*
** Convert the list to a string.
*/
static int list_tostring(lua_State *L) {
  luaL_Buffer b;
  lua_Integer len = luaL_len(L, 1);
  luaL_buffinit(L, &b);
  /* Prefix the string with name from metatable */
  if (luaL_getmetafield(L, 1, "__name") != LUA_TNIL) {
    luaL_addvalue(&b);
    luaL_addchar(&b, ' ');
  }
  luaL_addchar(&b, '{');
  lua_Integer i = 1;
  for (; i < len; i++) {
    addfield(L, &b, i);
    luaL_addstring(&b, ", ");
  }
  if (i == len)  /* add last value (if interval was not empty) */
    addfield(L, &b, i);
  luaL_addchar(&b, '}');
  luaL_pushresult(&b);
  return 1;
}

/*
** Pushes the standard `table` module to the stack.
*/
static void pushtablemodule (lua_State *L) {
  lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
  if (!lua_getfield(L, -1, LUA_TABLIBNAME)) {
    /* apparently it's not been loaded yes. So open it here (but don't
     * 'load' it). */
    lua_pushcfunction(L, luaopen_table);
    lua_pushliteral(L, LUA_TABLIBNAME);
    lua_call(L, 1, 1);
  }
  lua_remove(L, -2);  /* remove LOADED table */
}

/*
** Fields to copy from standard `table` package.
*/
static const char *tablelib_functions[] = {
  "insert",
  "remove",
  "sort",
  NULL
};

/*
** Copy fields from standard `table` module to the table at the given
** index.
*/
static void copyfromtablelib (lua_State *L, int idx) {
  int absidx = lua_absindex(L, idx);
  pushtablemodule(L);
  for (const char **name = tablelib_functions; *name != NULL; *name++) {
    if (lua_getfield(L, -1, *name)) {
      lua_setfield(L, absidx, *name);
    } else {
      lua_pop(L, 1);
    }
  }
  lua_pop(L, 1);  /* remove table module */
}

static const luaL_Reg list_funcs[] = {
  {"__concat", list_concat},
  {"__eq", list_eq},
  {"__tostring", list_tostring},
  {"at", list_at},
  {"clone", list_clone},
  {"extend", list_extend},
  {"filter", list_filter},
  {"find", list_find},
  {"find_if", list_find_if},
  {"includes", list_includes},
  {"insert", missing},
  {"map", list_map},
  {"new", list_new},
  {"remove", missing},
  {"sort", missing},
  {NULL, NULL}
};

static const luaL_Reg metareg[] = {
  {"__call", list_new},
  {NULL, NULL}
};

/*
** Creates a new metatable for a new List-like type.
 */
int lualist_newmetatable (lua_State *L, const char *name) {
  if (luaL_newmetatable(L, name)) {
    luaL_setfuncs(L, list_funcs, 0);
    /* use functions from standard table module. */
    copyfromtablelib(L, -1);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
    return 1;
  }
  return 0;
}

int luaopen_list (lua_State *L) {
  luaL_checkversion(L);
  lualist_newmetatable(L, LIST_T);

  lua_newtable(L);
  luaL_setfuncs(L, metareg, 0);
  lua_setmetatable(L, -2);
  return 1;
}
