{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : HsLua.Packaging.Convenience
Copyright   : © 2021-2022 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>

Convenience functions for common parameter and result types.
-}
module HsLua.Packaging.Convenience
where

import Data.Text (Text)
import HsLua.Marshalling
import HsLua.Packaging.Function

-- * Parameters

-- | Defines a function parameter of type 'Bool'.
boolParam :: Text -- ^ parameter name
          -> Text -- ^ parameter description
          -> Parameter e Bool
boolParam = parameter peekBool "boolean"
{-# INLINE boolParam #-}

-- | Defines a function parameter for an integral type.
integralParam :: (Read a, Integral a)
              => Text -- ^ parameter name
              -> Text -- ^ parameter description
              -> Parameter e a
integralParam = parameter peekIntegral "integer"
{-# INLINE integralParam #-}

-- | Defines a function parameter of type 'String'.
stringParam :: Text -- ^ parameter name
            -> Text -- ^ parameter description
            -> Parameter e String
stringParam = parameter peekString "string"
{-# INLINE stringParam #-}

-- | Defines a function parameter of type 'Text'.
textParam :: Text -- ^ parameter name
          -> Text -- ^ parameter description
          -> Parameter e Text
textParam = parameter peekText "string"
{-# INLINE textParam #-}


-- * Results

-- | Defines a function result of type 'Bool'.
boolResult :: Text -- ^ result description
           -> FunctionResults e Bool
boolResult = functionResult pushBool "boolean"
{-# INLINE boolResult #-}

-- | Defines a function result for an integral type.
integralResult :: (Integral a, Show a)
               => Text -- ^ result description
               -> FunctionResults e a
integralResult = functionResult pushIntegral "integer|string"
{-# INLINE integralResult #-}

-- | Defines a function result of type 'Text'.
stringResult :: Text -- ^ result description
             -> FunctionResults e String
stringResult = functionResult pushString "string"
{-# INLINE stringResult #-}

-- | Defines a function result of type 'Text'.
textResult :: Text -- ^ result description
           -> FunctionResults e Text
textResult = functionResult pushText "string"
{-# INLINE textResult #-}
