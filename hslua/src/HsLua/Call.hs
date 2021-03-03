{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : HsLua.Call
Copyright   : © 2020-2021 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : alpha
Portability : Portable

Marshaling and documenting Haskell functions.
-}
module HsLua.Call
  ( DocumentedFunction (..)
  , toHsFnPrecursor
  , toHsFnPrecursorWithStartIndex
  , applyParameter
  , returnResult
  , Parameter (..)
  , FunctionResult (..)
  , FunctionResults
    -- * Operators
  , (<#>)
  , (=#>)
  , (#?)
    -- * Documentation
  , FunctionDoc (..)
  , ParameterDoc (..)
  , FunctionResultDoc (..)
  , render
    -- * Pushing to Lua
  , pushDocumentedFunction
    -- * Convenience functions
  , parameter
  , optionalParameter
  , functionResult
  ) where

import Control.Monad.Except
import Data.Text (Text)
import HsLua.Core hiding (HaskellFunction, pushHaskellFunction)
import HsLua.Peek
import HsLua.Push
import qualified Data.Text as T
import qualified HsLua.Core as Lua

-- | Lua operation with an explicit error type and state (i.e.,
-- without exceptions).
type LuaExcept a = ExceptT PeekError Lua a


--
-- Function components
--

-- | Result of a call to a Haskell function.
data FunctionResult a
  = FunctionResult
  { fnResultPusher :: Pusher a
  , fnResultDoc :: FunctionResultDoc
  }

-- | List of function results in the order in which they are
-- returned in Lua.
type FunctionResults a = [FunctionResult a]

-- | Function parameter.
data Parameter a = Parameter
  { parameterPeeker :: Peeker a
  , parameterDoc    :: ParameterDoc
  }

-- | Haskell equivallent to CFunction, i.e., function callable
-- from Lua.
data DocumentedFunction = DocumentedFunction
  { callFunction :: Lua NumResults
  , functionDoc :: Maybe FunctionDoc
  }

--
-- Documentation
--

-- | Documentation for a Haskell function
data FunctionDoc = FunctionDoc
  { functionDescription :: Text
  , parameterDocs       :: [ParameterDoc]
  , functionResultDocs  :: [FunctionResultDoc]
  }
  deriving (Eq, Ord, Show)

-- | Documentation for function parameters.
data ParameterDoc = ParameterDoc
  { parameterName :: Text
  , parameterType :: Text
  , parameterDescription :: Text
  , parameterIsOptional :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Documentation for the result of a function.
data FunctionResultDoc = FunctionResultDoc
  { functionResultType :: Text
  , functionResultDescription :: Text
  }
  deriving (Eq, Ord, Show)


--
-- Haskell function building
--

-- | Helper type used to create 'HaskellFunction's.
data HsFnPrecursor a = HsFnPrecursor
  { hsFnPrecursorAction :: LuaExcept a
  , hsFnMaxParameterIdx :: StackIndex
  , hsFnParameterDocs :: [ParameterDoc]
  }
  deriving (Functor)

-- | Create a HaskellFunction precursor from a pure function.
toHsFnPrecursor :: a -> HsFnPrecursor a
toHsFnPrecursor = toHsFnPrecursorWithStartIndex (StackIndex 0)

toHsFnPrecursorWithStartIndex :: StackIndex -> a -> HsFnPrecursor a
toHsFnPrecursorWithStartIndex idx f = HsFnPrecursor
  { hsFnPrecursorAction = return f
  , hsFnMaxParameterIdx = idx
  , hsFnParameterDocs = mempty
  }

-- | Partially apply a parameter.
applyParameter :: HsFnPrecursor (a -> b)
               -> Parameter a
               -> HsFnPrecursor b
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
returnResults :: HsFnPrecursor a
              -> FunctionResults a
              -> DocumentedFunction
returnResults bldr fnResults = DocumentedFunction
  { callFunction = do
      hsResult <- runExceptT $ hsFnPrecursorAction bldr
      case hsResult of
        Left err -> do
          pushString $ formatPeekError err
          Lua.error
        Right x -> do
          forM_ fnResults $ \(FunctionResult push _) -> push x
          return $ NumResults (fromIntegral $ length fnResults)

  , functionDoc = Just $ FunctionDoc
    { functionDescription = ""
    , parameterDocs = reverse $ hsFnParameterDocs bldr
    , functionResultDocs = map fnResultDoc fnResults
    }
  }

-- | Like @'returnResult'@, but returns only a single result.
returnResult :: HsFnPrecursor a
             -> FunctionResult a
             -> DocumentedFunction
returnResult bldr = returnResults bldr . (:[])

-- | Updates the description of a Haskell function. Leaves the function
-- unchanged if it has no documentation.
updateFunctionDescription :: DocumentedFunction -> Text -> DocumentedFunction
updateFunctionDescription fn desc =
  case functionDoc fn of
    Nothing -> fn
    Just fnDoc ->
      fn { functionDoc = Just $ fnDoc { functionDescription = desc} }

--
-- Operators
--

infixl 8 <#>, =#>, #?

-- | Inline version of @'applyParameter'@.
(<#>) :: HsFnPrecursor (a -> b)
      -> Parameter a
      -> HsFnPrecursor b
(<#>) = applyParameter

-- | Inline version of @'returnResult'@.
(=#>) :: HsFnPrecursor a
      -> FunctionResults a
      -> DocumentedFunction
(=#>) = returnResults

-- | Inline version of @'updateFunctionDescription'@.
(#?) :: DocumentedFunction -> Text -> DocumentedFunction
(#?) = updateFunctionDescription

--
-- Render documentation
--

render :: FunctionDoc -> Text
render (FunctionDoc desc paramDocs resultDoc) =
  (if T.null desc then "" else desc <> "\n\n") <>
  renderParamDocs paramDocs <>
  case resultDoc of
    [] -> ""
    rd -> "\nReturns:\n\n" <> T.intercalate "\n" (map renderResultDoc rd)

renderParamDocs :: [ParameterDoc] -> Text
renderParamDocs pds = "Parameters:\n\n" <>
  T.intercalate "\n" (map renderParamDoc pds)

renderParamDoc :: ParameterDoc -> Text
renderParamDoc pd = mconcat
  [ parameterName pd
  ,  "\n:   "
  , parameterDescription pd
  , " (", parameterType pd, ")\n"
  ]

renderResultDoc :: FunctionResultDoc -> Text
renderResultDoc rd = mconcat
  [ " - "
  , functionResultDescription rd
  , " (", functionResultType rd, ")\n"
  ]

--
-- Push to Lua
--

pushDocumentedFunction :: DocumentedFunction -> Lua ()
pushDocumentedFunction = Lua.pushHaskellFunction . callFunction

--
-- Convenience functions
--

-- | Creates a parameter.
parameter :: Peeker a     -- ^ method to retrieve value from Lua
          -> Text         -- ^ expected Lua type
          -> Text         -- ^ parameter name
          -> Text         -- ^ parameter description
          -> Parameter a
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
optionalParameter :: Peeker a     -- ^ method to retrieve the value from Lua
                  -> Text         -- ^ expected Lua type
                  -> Text         -- ^ parameter name
                  -> Text         -- ^ parameter description
                  -> Parameter (Maybe a)
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
functionResult :: Pusher a        -- ^ method to push the Haskell result to Lua
               -> Text            -- ^ Lua type of result
               -> Text            -- ^ result description
               -> FunctionResults a
functionResult pusher type_ desc = (:[]) $ FunctionResult
  { fnResultPusher = pusher
  , fnResultDoc = FunctionResultDoc
    { functionResultType = type_
    , functionResultDescription = desc
    }
  }
