-- GENERATED by C->Haskell Compiler, version 0.28.2 Switcheroo, 1 April 2016 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "src/Foreign/R/Parse.chs" #-}
-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Bindings for @<R/R_ext/Parse.h>@.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}



module Foreign.R.Parse
  ( parseVector
  , ParseStatus(..)
  ) where
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Ptr as C2HSImp



import Foreign.R.Constraints
import qualified Foreign.R as R
-- XXX Duplicate import to make c2hs happy. The problem is that c2hs doesn't
-- like the "as R" of the above import.
import Foreign.R
{-# LINE 25 "src/Foreign/R/Parse.chs" #-}


import Foreign
import Foreign.C

-- | The return code of a call to 'parseVector', indicating whether the parser
-- failed or succeeded.
data ParseStatus = PARSE_NULL
                 | PARSE_OK
                 | PARSE_INCOMPLETE
                 | PARSE_ERROR
                 | PARSE_EOF
  deriving (Enum,Eq,Show)

{-# LINE 32 "src/Foreign/R/Parse.chs" #-}


-- | @parseVector text num status source@ parses the input string into an AST.
-- @source@, if provided, names the origin of @text@ (e.g. a filename). @num@
-- limits the number of expressions to parse, or @-1@ if no limit.

-- TODO: use ParseStatus or write a wrapper for parseVector.
parseVector :: (In a [R.Nil, R.String]) => (SEXP s (R.String)) -> (Int) -> (Ptr CInt) -> (SEXP s a) -> IO ((SEXP s (R.Expr)))
parseVector a1 a2 a3 a4 =
  let {a1' = unsexp a1} in 
  let {a2' = fromIntegral a2} in 
  let {a3' = id a3} in 
  let {a4' = unsexp a4} in 
  parseVector'_ a1' a2' a3' a4' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 45 "src/Foreign/R/Parse.chs" #-}


foreign import ccall safe "Foreign/R/Parse.chs.h R_ParseVector"
  parseVector'_ :: ((SEXP0) -> (C2HSImp.CInt -> ((C2HSImp.Ptr C2HSImp.CInt) -> ((SEXP0) -> (IO (SEXP0))))))