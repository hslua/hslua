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
    -- * Creating documented functions
  , defun
  , applyParameter
  , returnResult
  , liftPure
  , liftPure2
  , liftPure3
  , liftPure4
  , liftPure5
    -- ** Types
  , Parameter (..)
  , FunctionResult (..)
  , FunctionResults
    -- ** Operators
  , (###)
  , (<#>)
  , (=#>)
  , (#?)
    -- * Modifying functions
  , setName
  , since
    -- * Pushing to Lua
  , pushDocumentedFunction
    -- * Accessing documentation in Lua
  , docsField
  , pushDocumentation
    -- * Convenience functions
  , parameter
  , optionalParameter
  , functionResult
    -- * Internal
  , toHsFnPrecursor
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
  , hsFnName :: Name
  }
  deriving (Functor)

-- | Result of a call to a Haskell function.
data FunctionResult e a
  = FunctionResult
  { fnResultPusher :: Pusher e a
  , fnResultDoc :: FunctionResultDoc
  }

-- | List of function results in the order in which they are
-- returned in Lua.
type FunctionResults e a = [FunctionResult e a]

-- | Function parameter.
data Parameter e a = Parameter
  { parameterPeeker :: Peeker e a
  , parameterDoc    :: ParameterDoc
  }


-- | Begin wrapping a monadic Lua function such that it can be turned
-- into a documented function exposable to Lua.
defun :: Name -> a -> HsFnPrecursor e a
defun = toHsFnPrecursor (StackIndex 0)

-- | Turns a pure function into a monadic Lua function.
liftPure :: (a -> b)
         -> (a -> LuaE e b)
liftPure f = return . f

-- | Turns a binary function into a Lua function.
liftPure2 :: (a -> b -> c)
          -> (a -> b -> LuaE e c)
liftPure2 f a b = return (f a b)

-- | Turns a ternary function into a Lua function.
liftPure3 :: (a -> b -> c -> d)
          -> (a -> b -> c -> LuaE e d)
liftPure3 f a b c = return (f a b c)

-- | Turns a quarternary function into a Lua function.
liftPure4 :: (a -> b -> c -> d -> e)
          -> (a -> b -> c -> d -> LuaE err e)
liftPure4 f a b c d = return (f a b c d)

-- | Turns a quinary function into a Lua function.
liftPure5 :: (a -> b -> c -> d -> e -> f)
          -> (a -> b -> c -> d -> e -> LuaE err f)
liftPure5 f a b c d e = return (f a b c d e)

-- | Create a HaskellFunction precursor from a monadic function,
-- selecting the stack index after which the first function parameter
-- will be placed.
toHsFnPrecursor :: StackIndex -> Name -> a -> HsFnPrecursor e a
toHsFnPrecursor idx name f = HsFnPrecursor
  { hsFnPrecursorAction = return f
  , hsFnMaxParameterIdx = idx
  , hsFnParameterDocs = mempty
  , hsFnName = name
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
  bldr
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

  , functionName = hsFnName bldr
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

-- | Renames a documented function.
setName :: Name -> DocumentedFunction e -> DocumentedFunction e
setName name fn = fn { functionName = name }

-- | Sets the library version at which the function was introduced in its
-- current form.
since :: DocumentedFunction e -> Version -> DocumentedFunction e
since fn version =
  let fnDoc = functionDoc fn
  in fn { functionDoc = fnDoc { functionSince = Just version  }}

--
-- Operators
--

infixl 8 ###, <#>, =#>, #?, `since`

-- | Like '($)', but left associative.
(###) :: (a -> HsFnPrecursor e a) -> a -> HsFnPrecursor e a
(###) = ($)

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
