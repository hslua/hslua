local function catch_haskell(ret, err_msg)
    if ret == _HASKELLERR then
      -- `error` call adds a prefix to error message
      error(err_msg)
    end
    return ret
end

function fail_when_zero(n)
  print("Lua: " .. tostring(n))
  if n == 0 then
    error("Failing from Lua")
  end
  return catch_haskell(fail_when_zero_haskell(n - 1))
end
