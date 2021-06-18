{-# LANGUAGE OverloadedStrings #-}
{-| Santa's wishlist processor.

The program expexts the path to a filter file as its only argument.
See the blog-post for more details:
https://hslua.org/santas-little-lua-scripts.html
-}
module Main where
import Control.Monad (filterM)
import Data.Text (Text, pack)
import HsLua.Core ( Lua, NumResults (..), NumArgs (..)
                  , call, dofile, newtable, nth, pop, pushvalue, run
                  , setfield, top, toboolean)
import HsLua.Marshalling (pushBool, pushText)
import System.Environment (getArgs)

data Toy = Bricks | TrainSet | Doll deriving Show
data Behavior = Nice | Naughty deriving (Eq, Show)

data Wish = Wish
  { wishingChild :: Child
  , wishedToy :: Toy
  } deriving Show

data Child = Child
  { childName     :: Text
  , childBehavior :: Behavior
  } deriving Show

pushToy :: Toy -> Lua ()
pushToy = pushText . pack . show

pushChild :: Child -> Lua ()
pushChild (Child name behavior) = do
  -- create new Lua table on the stack
  newtable
  -- push boolean to stack
  pushText name
  -- table now in position 2; assign string to field in table
  setfield (nth 2) "name"

  -- push boolean to stack
  pushBool (behavior == Nice)
  setfield (nth 2) "nice"

pushWish :: Wish -> Lua ()
pushWish (Wish child toy) = do
  newtable
  pushChild child
  setfield (nth 2) "child"
  pushToy toy
  setfield (nth 2) "toy"

wishes :: [Wish]
wishes =
  [ Wish (Child "Theodor" Nice) Bricks
  , Wish (Child "Philine" Nice) TrainSet
  , Wish (Child "Steve" Naughty) Doll
  ]

hasPredicate :: Wish -> Lua Bool
hasPredicate wish = do
  -- Assume filter function is at the top of the stack;
  -- create a copy so we can re-use it.
  pushvalue top
  pushWish wish
  -- Call the function. There is one argument on the stack,
  -- and we expect one result to be returned.
  call (NumArgs 1) (NumResults 1)
  toboolean top <* pop 1

main :: IO ()
main = do
  filterFile <- fmap (!! 0) getArgs -- get first argument
  result <- run $ do
    _status <- dofile filterFile
    filterM hasPredicate wishes
  print result
