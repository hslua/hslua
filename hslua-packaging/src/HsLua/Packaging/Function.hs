{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Function
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : Portable

Marshaling and documenting Haskell functions.
-}
module HsLua.Packaging.Function
  ( DocumentedFunction (..)
  , toHsFnPrecursor
  , toHsFnPrecursorWithStartIndex
  , defun
  , applyParameter
  , returnResult
  , since
    -- * Operators
  , (<#>)
  , (=#>)
  , (#?)
    -- * Pushing to Lua
  , pushDocumentedFunction
    -- * Accessing documentation in Lua
  , docsField
  , pushDocumentation
    -- * Convenience functions
  , parameter
  , optionalParameter
  , functionResult
  ) where

import Control.Monad.Except
import Data.Text (Text)
import Data.Version (Version)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging.Rendering (renderFunction)
import HsLua.Packaging.Types
import qualified HsLua.Core as Lua

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

--
-- Haskell function building
--

-- | Lua operation with an additional failure mode that can stack errors
-- from different contexts; errors are not based on exceptions).
type LuaExcept e a = ExceptT PeekError (LuaE e) a

-- | Helper type used to create 'HaskellFunction's.
data HsFnPrecursor e a = HsFnPrecursor
  { hsFnPrecursorAction :: LuaExcept e a
  , hsFnMaxParameterIdx :: StackIndex
  , hsFnParameterDocs :: [ParameterDoc]
  }
  deriving (Functor)

-- | Create a HaskellFunction precursor from a pure function.
toHsFnPrecursor :: a -> HsFnPrecursor e a
toHsFnPrecursor = toHsFnPrecursorWithStartIndex (StackIndex 0)

-- | Create a HaskellFunction precursor from a pure function, selecting
-- the stack index after which the first function parameter will be
-- placed.
toHsFnPrecursorWithStartIndex :: StackIndex -> a -> HsFnPrecursor e a
toHsFnPrecursorWithStartIndex idx f = HsFnPrecursor
  { hsFnPrecursorAction = return f
  , hsFnMaxParameterIdx = idx
  , hsFnParameterDocs = mempty
  }

-- | Partially apply a parameter.
applyParameter :: HsFnPrecursor e (a -> b)
               -> Parameter e a
               -> HsFnPrecursor e b
applyParameter bldr param = do
  let action = hsFnPrecursorAction bldr
  let i = hsFnMaxParameterIdx bldr + 1
  let context = "retrieving function argument " <>
        (parameterName . parameterDoc) param
  let nextAction f = withExceptT (pushMsg context) $ do
        x <- ExceptT $ parameterPeeker param i
        return $ f x
  HsFnPrecursor
    { hsFnPrecursorAction = action >>= nextAction
    , hsFnMaxParameterIdx = i
    , hsFnParameterDocs = parameterDoc param : hsFnParameterDocs bldr
    }

-- | Take a 'HaskellFunction' precursor and convert it into a full
-- 'HaskellFunction', using the given 'FunctionResult's to return
-- the result to Lua.
returnResults :: HsFnPrecursor e (LuaE e a)
              -> FunctionResults e a
              -> DocumentedFunction e
returnResults bldr fnResults = DocumentedFunction
  { callFunction = do
      hsResult <- runExceptT $ hsFnPrecursorAction bldr
      case hsResult of
        Left err -> do
          pushString $ formatPeekError err
          Lua.error
        Right x -> do
          result <- x
          forM_ fnResults $ \(FunctionResult push _) -> push result
          return $! NumResults (fromIntegral $ length fnResults)

  , functionName = ""
  , functionDoc = FunctionDoc
    { functionDescription = ""
    , parameterDocs = reverse $ hsFnParameterDocs bldr
    , functionResultDocs = map fnResultDoc fnResults
    , functionSince = Nothing
    }
  }

-- | Like @'returnResult'@, but returns only a single result.
returnResult :: HsFnPrecursor e (LuaE e a)
             -> FunctionResult e a
             -> DocumentedFunction e
returnResult bldr = returnResults bldr . (:[])

-- | Updates the description of a Haskell function. Leaves the function
-- unchanged if it has no documentation.
updateFunctionDescription :: DocumentedFunction e
                          -> Text
                          -> DocumentedFunction e
updateFunctionDescription fn desc =
  let fnDoc = functionDoc fn
  in fn { functionDoc = fnDoc { functionDescription = desc} }

defun :: Name -> DocumentedFunction e -> DocumentedFunction e
defun name fn = fn { functionName = name }

-- | Sets the library version at which the function was introduced in its
-- current form.
since :: DocumentedFunction e -> Version -> DocumentedFunction e
since fn version =
  let fnDoc = functionDoc fn
  in fn { functionDoc = fnDoc { functionSince = Just version  }}

--
-- Operators
--

infixl 8 <#>, =#>, #?, `since`

-- | Inline version of @'applyParameter'@.
(<#>) :: HsFnPrecursor e (a -> b)
      -> Parameter e a
      -> HsFnPrecursor e b
(<#>) = applyParameter

-- | Inline version of @'returnResults'@.
(=#>) :: HsFnPrecursor e (LuaE e a)
      -> FunctionResults e a
      -> DocumentedFunction e
(=#>) = returnResults

-- | Inline version of @'updateFunctionDescription'@.
(#?) :: DocumentedFunction e -> Text -> DocumentedFunction e
(#?) = updateFunctionDescription

--
-- Push to Lua
--

-- | Name of the registry field holding the documentation table. The
-- documentation table is indexed by the documented objects, like module
-- tables and functions, and contains documentation strings as values.
--
-- The table is an ephemeron table, i.e., an entry gets garbage
-- collected if the key is no longer reachable.
docsField :: Name
docsField = "HsLua docs"

-- | Pushes a documented Haskell function to the Lua stack, making it
-- usable as a normal function in Lua. At the same time, the function
-- docs are registered in the documentation table.
pushDocumentedFunction :: LuaError e
                       => DocumentedFunction e -> LuaE e ()
pushDocumentedFunction fn = do
  -- push function
  Lua.pushHaskellFunction $ callFunction fn

  -- store documentation
  Lua.getfield registryindex docsField >>= \case
    TypeTable -> return () -- already have the documentation table
    _ -> do
      Lua.pop 1            -- pop non-table value
      Lua.newtable         -- create documentation table
      Lua.pushstring "k"   -- Make it an "ephemeron table" and..
      Lua.setfield (nth 2) "__mode"  -- collect docs if function is GCed
      Lua.pushvalue top    -- add copy of table to registry
      Lua.setfield registryindex docsField
  Lua.pushvalue (nth 2)  -- the function
  pushText $ renderFunction fn
  Lua.rawset (nth 3)
  Lua.pop 1              -- pop doc table, leave function on stack

-- | Pushes the documentation of the object at the given index to the
-- stack, or just *nil* if no documentation is available.
pushDocumentation :: LuaError e => StackIndex -> LuaE e NumResults
pushDocumentation idx = do
  idx' <- Lua.absindex idx
  Lua.getfield registryindex docsField >>= \case
    TypeTable -> do
      Lua.pushvalue idx'
      Lua.rawget (nth 2)
    _ -> do -- no documentation table available
      Lua.pop 1    -- pop contents of docsField
      Lua.pushnil
  return (NumResults 1)

--
-- Convenience functions
--

-- | Creates a parameter.
parameter :: Peeker e a   -- ^ method to retrieve value from Lua
          -> Text         -- ^ expected Lua type
          -> Text         -- ^ parameter name
          -> Text         -- ^ parameter description
          -> Parameter e a
parameter peeker type_ name desc = Parameter
  { parameterPeeker = peeker
  , parameterDoc = ParameterDoc
    { parameterName = name
    , parameterDescription = desc
    , parameterType = type_
    , parameterIsOptional = False
    }
  }

-- | Creates an optional parameter.
optionalParameter :: Peeker e a   -- ^ method to retrieve the value from Lua
                  -> Text         -- ^ expected Lua type
                  -> Text         -- ^ parameter name
                  -> Text         -- ^ parameter description
                  -> Parameter e (Maybe a)
optionalParameter peeker type_ name desc = Parameter
  { parameterPeeker = optional peeker
  , parameterDoc = ParameterDoc
    { parameterName = name
    , parameterDescription = desc
    , parameterType = type_
    , parameterIsOptional = True
    }
  }

-- | Creates a function result.
functionResult :: Pusher e a      -- ^ method to push the Haskell result to Lua
               -> Text            -- ^ Lua type of result
               -> Text            -- ^ result description
               -> FunctionResults e a
functionResult pusher type_ desc = (:[]) $ FunctionResult
  { fnResultPusher = pusher
  , fnResultDoc = FunctionResultDoc
    { functionResultType = type_
    , functionResultDescription = desc
    }
  }
