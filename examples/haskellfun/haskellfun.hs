{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- An example with higher-level Haskell functions. Haskell functions are
-- wrapped by hslua automatically for ensuring argument types and nubmers
-- and passing arguments from Lua stack to Haskell functions. Return values
-- are also handled by hslua automatically(so you don't put return value to
-- stack manually).

import qualified Data.ByteString as B
import           Data.Monoid
import           Scripting.Lua   as Lua

main :: IO ()
main = do
    l <- newstate
    openlibs l
    registerhsfunction l "concat" concat'
    registerhsfunction l "pow" pow
    registerhsfunction l "helloWorld" helloWorld
    loadfile l "haskellfun.lua"
    call l 0 0
    close l

concat' :: B.ByteString -> B.ByteString -> IO B.ByteString
concat' s1 s2 = return $ s1 <> s2

pow :: Double -> Double -> IO Double
pow d1 d2 = return $ d1 ** d2

helloWorld :: IO B.ByteString
helloWorld = return "Hello, World!"
