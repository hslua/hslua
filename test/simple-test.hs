import qualified Scripting.Lua as Lua
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as Utf8

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.openlibs l
    Lua.callproc l "print" "Hello from Lua"

    Lua.pushstring l "A\NULB"
    putStrLn =<< Lua.tostring l (-1)
    Lua.remove l (-1)

    Lua.pushbytestring l (Utf8.fromString "A\NUL中文B")
    putStrLn . Utf8.toString =<< Lua.tobytestring l (-1)
    Lua.remove l (-1)

    Lua.close l
