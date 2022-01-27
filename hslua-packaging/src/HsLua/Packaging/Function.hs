{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Function
Copyright   : © 2020-2022 Albert Krewinkel
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
  , lambda
  , applyParameter
  , returnResult
  , returnResults
  , returnResultsOnStack
  , updateFunctionDescription
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
  , (=?>)
  , (#?)
    -- * Modifying functions
  , setName
  , since
    -- * Pushing to Lua
  , pushDocumentedFunction
    -- * Convenience functions
  , parameter
  , opt
  , optionalParameter
  , functionResult
    -- * Internal
  , HsFnPrecursor
  , toHsFnPrecursor
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Data.Text (Text)
import Data.Version (Version)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging.Documentation
import HsLua.Packaging.Types
import qualified HsLua.Core as Lua
import qualified HsLua.Core.Utf8 as Utf8

#if !MIN_VERSION_base(4,12,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

--
-- Haskell function building
--

-- | Helper type used to create 'HaskellFunction's.
data HsFnPrecursor e a = HsFnPrecursor
  { hsFnPrecursorAction :: Peek e a
  , hsFnMaxParameterIdx :: StackIndex
  , hsFnParameterDocs :: [ParameterDoc]
  , hsFnName :: Name
  }
  deriving (Functor)

-- | Result of a call to a Haskell function.
data FunctionResult e a
  = FunctionResult
  { fnResultPusher  :: Pusher e a
  , fnResultDoc     :: ResultValueDoc
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

-- | Just like @defun@, but uses an empty name for the documented
-- function. Should be used when defining methods or operators.
lambda :: a -> HsFnPrecursor e a
lambda = defun (Name mempty)

-- | Turns a pure function into a monadic Lua function.
--
-- The resulting function is strict.
liftPure :: (a -> b)
         -> (a -> LuaE e b)
liftPure f !a = return $! f a

-- | Turns a binary function into a Lua function.
--
-- The resulting function is strict in both its arguments.
liftPure2 :: (a -> b -> c)
          -> (a -> b -> LuaE e c)
liftPure2 f !a !b = return $! f a b

-- | Turns a ternary function into a Lua function.
--
-- The resulting function is strict in all of its arguments.
liftPure3 :: (a -> b -> c -> d)
          -> (a -> b -> c -> LuaE e d)
liftPure3 f !a !b !c = return $! f a b c

-- | Turns a quarternary function into a Lua function.
--
-- The resulting function is strict in all of its arguments.
liftPure4 :: (a -> b -> c -> d -> e)
          -> (a -> b -> c -> d -> LuaE err e)
liftPure4 f !a !b !c !d = return $! f a b c d

-- | Turns a quinary function into a Lua function.
--
-- The resulting function is strict in all of its arguments.
liftPure5 :: (a -> b -> c -> d -> e -> f)
          -> (a -> b -> c -> d -> e -> LuaE err f)
liftPure5 f !a !b !c !d !e = return $! f a b c d e

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
  let context = Name . Utf8.fromText $ "function argument " <>
        (parameterName . parameterDoc) param
  let nextAction f = retrieving context $ do
        !x <- parameterPeeker param i
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
      hsResult <- runPeek
                . retrieving ("arguments for function " <> hsFnName bldr)
                $ hsFnPrecursorAction bldr
      case resultToEither hsResult of
        Left err -> do
          pushString err
          Lua.error
        Right x -> do
          result <- x
          forM_ fnResults $ \(FunctionResult push _) -> push result
          return $! NumResults (fromIntegral $ length fnResults)
  , functionName = hsFnName bldr
  , functionDoc = FunctionDoc
    { functionDescription = ""
    , parameterDocs = reverse $ hsFnParameterDocs bldr
    , functionResultsDocs = ResultsDocList $ map fnResultDoc fnResults
    , functionSince = Nothing
    }
  }

-- | Take a 'HaskellFunction' precursor and convert it into a full
-- 'HaskellFunction', using the given 'FunctionResult's to return
-- the result to Lua.
returnResultsOnStack :: HsFnPrecursor e (LuaE e NumResults)
                     -> Text
                     -> DocumentedFunction e
returnResultsOnStack bldr desc = DocumentedFunction
  { callFunction = do
      hsResult <- runPeek
                . retrieving ("arguments for function " <> hsFnName bldr)
                $ hsFnPrecursorAction bldr
      case resultToEither hsResult of
        Left err -> do
          pushString err
          Lua.error
        Right x -> x
  , functionName = hsFnName bldr
  , functionDoc = FunctionDoc
    { functionDescription = ""
    , parameterDocs = reverse $ hsFnParameterDocs bldr
    , functionResultsDocs = ResultsDocMult desc
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

infixl 8 ###, <#>, =#>, =?>, #?, `since`

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

-- | Return a flexible number of results that have been pushed by the
-- function action.
(=?>) :: HsFnPrecursor e (LuaE e NumResults)
      -> Text
      -> DocumentedFunction e
(=?>) = returnResultsOnStack

-- | Inline version of @'updateFunctionDescription'@.
(#?) :: DocumentedFunction e -> Text -> DocumentedFunction e
(#?) = updateFunctionDescription

--
-- Push to Lua
--

-- | Pushes a documented Haskell function to the Lua stack, making it
-- usable as a normal function in Lua. At the same time, the function
-- docs are registered in the documentation table.
pushDocumentedFunction :: LuaError e
                       => DocumentedFunction e -> LuaE e ()
pushDocumentedFunction fn = do
  Lua.pushHaskellFunction $ callFunction fn  -- push function
  pushFunctionDoc fn                         -- function documentation
  registerDocumentation (Lua.nth 2)          -- store documentation

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

-- | Makes a parameter optional.
opt :: Parameter e a -> Parameter e (Maybe a)
opt p = Parameter
  { parameterPeeker = \idx ->
      (Nothing <$ peekNoneOrNil idx) <|>
      (Just <$!> parameterPeeker p idx)
  , parameterDoc = (parameterDoc p){ parameterIsOptional = True }
  }

-- | Creates an optional parameter.
--
-- DEPRECATED: Use @opt (parameter ...)@ instead.
optionalParameter :: Peeker e a   -- ^ method to retrieve the value from Lua
                  -> Text         -- ^ expected Lua type
                  -> Text         -- ^ parameter name
                  -> Text         -- ^ parameter description
                  -> Parameter e (Maybe a)
optionalParameter peeker type_ name desc = opt $
  parameter peeker type_ name desc
{-# DEPRECATED optionalParameter "Use `opt (parameter ...)` instead." #-}

-- | Creates a function result.
functionResult :: Pusher e a      -- ^ method to push the Haskell result to Lua
               -> Text            -- ^ Lua type of result
               -> Text            -- ^ result description
               -> FunctionResults e a
functionResult pusher type_ desc = (:[]) $ FunctionResult
  { fnResultPusher = pusher
  , fnResultDoc = ResultValueDoc
                  { resultValueType = type_
                  , resultValueDescription = desc
                  }
  }
