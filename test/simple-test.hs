import qualified Scripting.Lua as Lua

main :: IO ()
main = do
     l <- Lua.newstate
     Lua.openlibs l
     Lua.callproc l "print" "Hello from Lua"
     Lua.close l
