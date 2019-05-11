------------------------------------------------------------------------
--- Assertors

local assertors = {}

local function register_assertor (name, callback, error_message)
  assertors[name] = function (...)
    local bool = callback(...)
    if bool then
      return
    end

    local success, formatted_message =
      pcall(string.format, error_message, ...)
    if not success then
      error('assertion failed, and error message could not be formatted', 2)
    end
    error('\n' .. formatted_message or 'assertion failed!', 2)
  end
end

--- Value is truthy
local function is_truthy (x)
  return x ~= false and x ~= nil
end

--- Value is falsy
local function is_falsy (x)
  return not is_truthy(x)
end

--- Value is nil
local function is_nil (x)
  return x == nil
end

--- Values are equal
local function are_equal (x, y)
  return x == y
end

local function cycle_aware_compare(t1, t2, cache)
  if cache[t1] and cache[t1][t2] then return true end

  local ty1 = type(t1)
  local ty2 = type(t2)

  -- if t1 == t2 then return true end
  if ty1 ~= ty2 then return false end
  if ty1 ~= 'table' then return t1 == t2 end

  -- Check tables have the same set of keys
  for k1 in pairs(t1) do
    if t2[k1] == nil then return false end
  end
  for k2 in pairs(t2) do
    if t1[k2] == nil then return false end
  end

  -- cache comparison result
  cache[t1] = cache[t1] or {}
  cache[t1][t2] = true

  for k1, v1 in pairs(t1) do
    local v2 = t2[k1]
    if not cycle_aware_compare(v1, v2, cache) then return false end
  end
  return true
end

--- Check if tables are the same
local function are_same(x, y)
  return cycle_aware_compare(x, y, {})
end

local function error_matches(fn, pattern)
  local success, msg = pcall(fn)
  if success then
    return false
  end
  return msg:match(pattern)
end

register_assertor('is_truthy', is_truthy, "expected a truthy value, got %s")
register_assertor('is_falsy', is_falsy, "expected a falsy value, got %s")
register_assertor('is_nil', is_nil, "expected nil, got %s")
register_assertor('are_same', are_same, 'expected same values, got %s and %s')
register_assertor(
  'are_equal',
  are_equal,
  "expected values to be equal, got '%s' and '%s'"
)
register_assertor(
  'error_matches',
  error_matches,
  'no error matching the given pattern was raised'
)

------------------------------------------------------------------------

local ok = true

local function test_success (name)
  return {
    name = name,
    result = ok,
  }
end

local function test_failure (name, err_msg)
  return {
    name = name,
    result = err_msg,
  }
end

------------------------------------------------------------------------
-- Test definitions

local function test_case (name, callback)
  local success, err_msg = pcall(callback)
  return success
    and test_success(name)
    or  test_failure(name, err_msg)
end

local function test_group (name, tests)
  if tests == nil then
    -- let's try to curry
    return function (tests_)
      return tests_ and test_group(name, tests_) or error('no tests')
    end
  end
  return {
    name = name,
    result = tests,
  }
end

return {
  assert = assertors,
  ok = ok,
  test_case = test_case,
  test_group = test_group
}
