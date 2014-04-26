local c1 = function ()
  print("callback 1")
end

local c2 = function ()
  print("callback 2")
end

local c3 = function ()
  print("callback 3")
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
callLuaCallbacks()
print("end")
