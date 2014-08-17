-- | added for issue #13
module Main where

import qualified Scripting.Lua               as Lua
import qualified Data.ByteString as B
import Data.Char (ord)

stringToByteString :: String -> B.ByteString
stringToByteString s = B.pack (map (\x -> fromIntegral (ord x)) s)

main :: IO ()
main = do
    l <- Lua.newstate
    Lua.pushstring2 l $ stringToByteString "A\NULB"
    B.putStrLn =<< Lua.tostring2 l 1
    Lua.close l
