local c1 = function ()
  print("callback 1")
  return "callback 1 return value"
end

local c2 = function ()
  print("callback 2")
  return false
end

local c3 = function ()
  print("callback 3")
  return 3
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
local callbackrets = callLuaCallbacks()
for _, v in ipairs(callbackrets) do
  print(v)
end
print("end")
