------------------------------------------------------------------------
--- Assertors

--- New assert object. Behaves like original `assert` when called, and
--- comes with many other tests.
local assert = setmetatable({}, {
  __call = _G.assert,  -- use global assert when called.
})

--- Special table allowing to use `assert.is.truthy` instead of
--- `assert.is_truthy.`
assert.is = setmetatable({}, {
    __index = function (t, k)
      return assert['is_' .. k]
    end
})

--- Special table allowing to use `assert.are.same` instead of
--- `assert.are_same.`
assert.are = setmetatable({}, {
    __index = function (t, k)
      return assert['are_' .. k]
    end
})

--- Special table allowing to use `assert.error.matches` instead of
--- `assert.error_matches.`
assert.error = setmetatable({}, {
    __index = function (t, k)
      return assert['error_' .. k]
    end
})

--- Create a new assertion function.
local function make_assertion (error_message, callback)
  return function (...)
    local assertion_holds, info = callback(...)
    -- Calling the assertion function produced an error, report it.
    if assertion_holds then
      return
    end

    -- Assertion failed, format and throw the error message
    local success, message
    if type(error_message) == 'function' then
      success, message = pcall(error_message, info, ...)
    elseif type(error_message) == 'string' then
      success, message = pcall(string.format, error_message, ...)
    else
      success, message = false, error_message
    end
    if not success then
      error('assertion failed, but error could not be formatted:\n' ..
            tostring(message), 1)
    end
    error('\n' .. message or 'assertion failed!', 2)
  end
end

--- Value is truthy
assert.is_truthy = make_assertion(
  "expected a truthy value, got %s",
  function (x)
    return x ~= false and x ~= nil
  end
)

--- Value is falsy
assert.is_falsy = make_assertion(
  "expected a falsy value, got %s",
  function (x)
    return not x
  end
)

--- Value is true
assert.is_true = make_assertion(
  "expected true, got %s",
  function (x)
    return x == true
  end
)

--- Value is false
assert.is_false = make_assertion(
  "expected false, got %s",
  function (x)
    return x == false
  end
)

--- Value is nil
assert.is_nil = make_assertion(
  "expected nil, got %s",
  function (x)
    return x == nil
  end
)

--- Values are equal
assert.are_equal = make_assertion(
  "expected values to be equal, got '%s' and '%s'",
  function (x, y)
    return x == y
  end
)

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
assert.are_same = make_assertion(
  'expected same values, got %s and %s',
  function (x, y)
    return cycle_aware_compare(x, y, {})
  end
)

--- Checks that a error is raised and that the error satisfies the given
-- assertion.
assert.error_satisfies = make_assertion(
  function (actual)
    return ('assertion did not hold for error object:%s')
      :format(actual)
  end,
  function (fn, assertion)
    local success, err = pcall(fn)
    if success then
      return false
    end
    return pcall(assertion, err)
  end
)

--- Checks that a error is raised and that the error equals an expected value.
assert.error_equals = make_assertion(
  function (actual, fun, expected)
    return ('expected error to equal %s, got: %s'):format(expected, actual)
  end,
  function (fn, expected)
    local success, err = pcall(fn)
    return not success and expected == err, err
  end
)

--- Checks that a error is raised and that the message matches the given
--- pattern.
assert.error_matches = make_assertion(
  function (actual, fun, expected)
    return ('expected error to match pattern \'%s\' but got: %s')
      :format(expected, actual)
  end,
  function (fn, pattern)
    local success, msg = pcall(fn)
    if success then
      return false
    end
    return tostring(msg):match(pattern), msg
  end
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
  assert = assert,
  ok = ok,
  test_case = test_case,
  test_group = test_group
}
