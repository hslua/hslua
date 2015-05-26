local function c1()
  print("callback 1")
  return "callback 1 return value"
end

local function c2()
  print("callback 2")
  return false
end

local function c3()
  print("callback 3")
  return 3
end

local function c_fails()
  print("Failing from Lua")
  error("failed")
end

print("begin")
addLuaCallbacks(c1, c2)
callLuaCallbacks()
print("reset callbacks")
resetLuaCallbacks()
callLuaCallbacks()
print("add callbacks in reverse order")
addLuaCallbacks(c3)
addLuaCallbacks(c2)
addLuaCallbacks(c1)
addLuaCallbacks(c_fails)
local callbackrets = callLuaCallbacks()
for _, v in ipairs(callbackrets) do
  print(v)
end
print("end")
