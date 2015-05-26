_HASKELLERR = {}

function catch_haskell(ret, err_msg)
    if ret == _HASKELLERR then
      print("Error caught from Haskell land: " .. err_msg)
      return
    end
    return ret
end

print(concat("hello", " world!"))
print(catch_haskell(pow(3.2, 5)))
print(catch_haskell(helloWorld()))
print(catch_haskell(pow("wrong")))
print(catch_haskell(pow(3)))
