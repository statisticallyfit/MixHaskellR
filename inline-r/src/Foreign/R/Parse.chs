-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/R_ext/Parse.h>@.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif

#include <Rinternals.h>
#include <R_ext/Parse.h>
module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where

import Foreign.R.Constraints
import qualified Foreign.R as R
-- XXX Duplicate import to make c2hs happy. The problem is that c2hs doesn't
-- like the "as R" of the above import.
{#import Foreign.R #}

import Foreign
import Foreign.C

-- | The return code of a call to 'parseVector', indicating whether the parser
-- failed or succeeded.
{#enum ParseStatus {} deriving (Eq, Show) #}

-- | @parseVector text num status source@ parses the input string into an AST.
-- @source@, if provided, names the origin of @text@ (e.g. a filename). @num@
-- limits the number of expressions to parse, or @-1@ if no limit.

-- TODO: use ParseStatus or write a wrapper for parseVector.
{#fun R_ParseVector as parseVector
  `(In a [R.Nil, R.String])'
  => { unsexp `SEXP s (R.String)'
     , `Int'
     , id `Ptr CInt'
     , unsexp `SEXP s a' }
  -> `SEXP s (R.Expr)' sexp #}
