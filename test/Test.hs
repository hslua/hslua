import qualified Scripting.Lua as Lua

hook :: Lua.LuaHook
hook _ _ = putStrLn "hook called"

main :: IO()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.sethook l hook 1
    Lua.callproc l "print" "Hello from Lua"
    Lua.close l
