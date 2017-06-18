{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- This custom Prelude emulates the API of the prelude
-- with base 4.8.

module Prelude
  ( module P
#if !MIN_VERSION_base(4,8,0)
  , Monoid(..)
  , Applicative(..)
  , (<$>)
  , (<$)
#endif
  )
where

import "base" Prelude as P
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
#endif
