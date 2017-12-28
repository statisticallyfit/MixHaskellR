-- GENERATED by C->Haskell Compiler, version 0.28.2 Switcheroo, 1 April 2016 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "src/Foreign/R.chs" #-}
-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Low-level bindings to core R datatypes and functions. Nearly all structures
-- allocated internally in R are instances of a 'SEXPREC'. A pointer to
-- a 'SEXPREC' is called a 'SEXP'.
--
-- To allow for precise typing of bindings to primitive R functions, we index
-- 'SEXP's by 'SEXPTYPE', which classifies the /form/ of a 'SEXP' (see
-- "Foreign.R.Type"). A function accepting 'SEXP' arguments of any type should
-- leave the type index uninstantiated. A function returning a 'SEXP' result of
-- unknown type should use 'SomeSEXP'. (More precisely, unknown types in
-- /negative/ position should be /universally/ quantified and unknown types in
-- /positive/ position should be /existentially/ quantified).
--
-- This module is intended to be imported qualified.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Necessary for c2hs < 0.26 compat.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- We don't use ticks in this module, because they confuse c2hs.
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Foreign.R
  ( module Foreign.R.Type
    -- * Internal R structures
  , SEXP(..)
  , SomeSEXP(..)
  , unSomeSEXP
    -- * Casts and coercions
    -- $cast-coerce
  , cast
  , asTypeOf
  , unsafeCoerce
    -- * Node creation
  , allocSEXP
  , allocList
  , allocVector
  , allocVectorProtected
  , install
  , mkString
  , mkChar
  , CEType(..)
  , mkCharCE
  , mkWeakRef
    -- * Node attributes
  , typeOf
  , isS4
  , setAttributes
  , getAttribute
  , getAttributes
    -- * Node accessor functions
    -- ** Lists
  , cons
  , lcons
  , car
  , cdr
  , tag
  , setCar
  , setCdr
  , setTag
    -- ** Environments
  , envFrame
  , envEnclosing
  , envHashtab
    -- ** Closures
  , closureFormals
  , closureBody
  , closureEnv
    -- ** Promises
  , promiseCode
  , promiseEnv
  , promiseValue
    -- ** Symbols
  , symbolPrintName
  , symbolValue
  , symbolInternal
    -- ** Vectors
  , length
  , trueLength
  , char
  , real
  , integer
  , logical
  , complex
  , raw
  , string
  , unsafeSEXPToVectorPtr
  , unsafeVectorPtrToSEXP
  , readVector
  , writeVector
    -- * Evaluation
  , eval
  , tryEval
  , tryEvalSilent
  , lang1
  , lang2
  , lang3
  , findFun
  , findVar
    -- * GC functions
  , protect
  , unprotect
  , unprotectPtr
  , preserveObject
  , releaseObject
  , gc
    -- * Globals
  , isRInteractive
  , nilValue
  , unboundValue
  , missingArg
  , baseEnv
  , emptyEnv
  , globalEnv
  , signalHandlers
  , interruptsPending
    -- * Communication with runtime
  , printValue
    -- * Low level info header access
  , SEXPInfo(..)
  , peekInfo
  , pokeInfo
  , mark
  , named
  -- * Internal types and functions
  --
  -- | Should not be used in user code. These exports are only needed for
  -- binding generation tools.
  , SEXPREC
  , SEXP0
  , sexp
  , unsexp
  , release
  , unsafeRelease
  , withProtected
  -- * Deprecated
  , indexVector
  ) where
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Ptr as C2HSImp



import Control.Memory.Region
import {-# SOURCE #-} Language.R.HExp (HExp)
import Foreign.R.Internal hiding (SEXP0)
import Foreign.R.Type
import Foreign.R.Type as R

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Exception (bracket)
import Control.Monad.Primitive ( unsafeInlineIO )
import Data.Bits -- For c2hs < 0.26.
import Data.Complex
import Data.Int (Int32)
import Data.Singletons (fromSing)
import Foreign (Ptr, castPtr, plusPtr, Storable(..))
import Foreign.C
import Prelude hiding (asTypeOf, length)








-- | 'SEXP' with no type index. This type and 'sexp' / 'unsexp'
-- are purely an artifact of c2hs (which doesn't support indexing a Ptr with an
-- arbitrary type in a @#pointer@ hook).
type SEXP0 = C2HSImp.Ptr (SEXPREC)
{-# LINE 188 "src/Foreign/R.chs" #-}


-- XXX temp workaround due to R bug: doesn't export R_CHAR when USE_RINTERNALS
-- is defined.
--------------------------------------------------------------------------------
-- Generic accessor functions                                                 --
--------------------------------------------------------------------------------

-- | read CAR object value
car :: (SEXP s a) -> IO ((SomeSEXP s))
car a1 =
  let {a1' = unsexp a1} in 
  car'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 200 "src/Foreign/R.chs" #-}


-- | read CDR object
cdr :: (SEXP s a) -> IO ((SomeSEXP s))
cdr a1 =
  let {a1' = unsexp a1} in 
  cdr'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 203 "src/Foreign/R.chs" #-}


-- | read object`s Tag
tag :: (SEXP s a) -> IO ((SomeSEXP s))
tag a1 =
  let {a1' = unsexp a1} in 
  tag'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')
  --- XXX: add better constraint

--------------------------------------------------------------------------------
-- Environment functions                                                      --
--------------------------------------------------------------------------------

-- | Environment frame.
envFrame :: (SEXP s R.Env) -> IO ((SEXP s R.PairList))
envFrame a1 =
  let {a1' = unsexp a1} in 
  envFrame'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 213 "src/Foreign/R.chs" #-}


-- | Enclosing environment.
envEnclosing :: (SEXP s R.Env) -> IO ((SEXP s R.Env))
envEnclosing a1 =
  let {a1' = unsexp a1} in 
  envEnclosing'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 216 "src/Foreign/R.chs" #-}


-- | Hash table associated with the environment, used for faster name lookups.
envHashtab :: (SEXP s R.Env) -> IO ((SEXP s R.Vector))
envHashtab a1 =
  let {a1' = unsexp a1} in 
  envHashtab'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 219 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Closure functions                                                          --
--------------------------------------------------------------------------------

-- | Closure formals (aka the actual arguments).
closureFormals :: (SEXP s R.Closure) -> IO ((SEXP s R.PairList))
closureFormals a1 =
  let {a1' = unsexp a1} in 
  closureFormals'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 226 "src/Foreign/R.chs" #-}


-- | The code of the closure.
closureBody :: (SEXP s R.Closure) -> IO ((SomeSEXP s))
closureBody a1 =
  let {a1' = unsexp a1} in 
  closureBody'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 229 "src/Foreign/R.chs" #-}


-- | The environment of the closure.
closureEnv :: (SEXP s R.Closure) -> IO ((SEXP s R.Env))
closureEnv a1 =
  let {a1' = unsexp a1} in 
  closureEnv'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 232 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Promise functions                                                          --
--------------------------------------------------------------------------------

-- | The code of a promise.
promiseCode :: (SEXP s R.Promise) -> IO ((SomeSEXP s))
promiseCode a1 =
  let {a1' = unsexp a1} in 
  promiseCode'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 239 "src/Foreign/R.chs" #-}


-- | The environment in which to evaluate the promise.
promiseEnv :: (SEXP s R.Promise) -> IO ((SEXP s R.Env))
promiseEnv a1 =
  let {a1' = unsexp a1} in 
  promiseEnv'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 242 "src/Foreign/R.chs" #-}


-- | The value of the promise, if it has already been forced.
promiseValue :: (SEXP s R.Promise) -> IO ((SomeSEXP s))
promiseValue a1 =
  let {a1' = unsexp a1} in 
  promiseValue'_ a1' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 245 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Vector accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read True Length vector field.
trueLength :: R.IsVector a => (SEXP s a) -> IO ((CInt))
trueLength a1 =
  let {a1' = unsexp a1} in 
  trueLength'_ a1' >>= \res ->
  let {res' = id res} in
  return (res')

{-# LINE 252 "src/Foreign/R.chs" #-}


-- | Read character vector data
char :: (SEXP s R.Char) -> IO ((CString))
char a1 =
  let {a1' = unsexp a1} in 
  char'_ a1' >>= \res ->
  let {res' = id res} in
  return (res')

{-# LINE 255 "src/Foreign/R.chs" #-}

-- XXX: check if we really need Word8 here, maybe some better handling of
-- encoding

-- | Read real vector data.
real :: (SEXP s R.Real) -> IO ((Ptr Double))
real a1 =
  let {a1' = unsexp a1} in 
  real'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 260 "src/Foreign/R.chs" #-}


-- | Read integer vector data.
integer :: (SEXP s R.Int) -> IO ((Ptr Int32))
integer a1 =
  let {a1' = unsexp a1} in 
  integer'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 263 "src/Foreign/R.chs" #-}


-- | Read raw data.
raw :: (SEXP s R.Raw) -> IO ((Ptr CChar))
raw a1 =
  let {a1' = unsexp a1} in 
  raw'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 266 "src/Foreign/R.chs" #-}


-- XXX Workaround c2hs syntax limitations.
type RLogical = 'R.Logical

-- | Read logical vector data.
logical :: (SEXP s RLogical) -> IO ((Ptr R.Logical))
logical a1 =
  let {a1' = unsexp a1} in 
  logical'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 272 "src/Foreign/R.chs" #-}


-- | Read complex vector data.
complex :: (SEXP s R.Complex) -> IO ((Ptr (Complex Double)))
complex a1 =
  let {a1' = unsexp a1} in 
  complex'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 276 "src/Foreign/R.chs" #-}


-- | Read string vector data.
string :: (SEXP s R.String) -> IO ((Ptr (SEXP s R.Char)))
string a1 =
  let {a1' = unsexp a1} in 
  string'_ a1' >>= \res ->
  let {res' = castPtr res} in
  return (res')

{-# LINE 280 "src/Foreign/R.chs" #-}


readVector :: R.IsGenericVector a => (SEXP s a) -> (Int) -> IO ((SomeSEXP s))
readVector a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = fromIntegral a2} in 
  readVector'_ a1' a2' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 284 "src/Foreign/R.chs" #-}


indexVector :: IsGenericVector a => SEXP s a -> Int -> IO (SomeSEXP s)
{-# DEPRECATED indexVector "Use readVector instead." #-}
indexVector = readVector

writeVector :: R.IsGenericVector a => (SEXP s a) -> (Int) -> (SEXP s b) -> IO ((SEXP s a))
writeVector a1 a2 a3 =
  let {a1' = unsexp a1} in 
  let {a2' = fromIntegral a2} in 
  let {a3' = unsexp a3} in 
  writeVector'_ a1' a2' a3' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 292 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Symbol accessor functions                                                  --
--------------------------------------------------------------------------------

-- | Read a name from symbol.
symbolPrintName :: (SEXP s R.Symbol) -> IO ((SEXP s R.Char))
symbolPrintName a1 =
  let {a1' = unsexp a1} in 
  symbolPrintName'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 299 "src/Foreign/R.chs" #-}


-- | Read value from symbol.
symbolValue :: (SEXP s R.Symbol) -> IO ((SEXP s a))
symbolValue a1 =
  let {a1' = unsexp a1} in 
  symbolValue'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 302 "src/Foreign/R.chs" #-}


-- | Read internal value from symbol.
symbolInternal :: (SEXP s R.Symbol) -> IO ((SEXP s a))
symbolInternal a1 =
  let {a1' = unsexp a1} in 
  symbolInternal'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 305 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Value conversion                                                           --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Value contruction                                                          --
--------------------------------------------------------------------------------

-- | Initialize a new string vector.
mkString :: (CString) -> IO ((SEXP V R.String))
mkString a1 =
  let {a1' = id a1} in 
  mkString'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 316 "src/Foreign/R.chs" #-}


-- | Initialize a new character vector (aka a string).
mkChar :: (CString) -> IO ((SEXP V R.Char))
mkChar a1 =
  let {a1' = id a1} in 
  mkChar'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 319 "src/Foreign/R.chs" #-}


-- | Create Character value with specified encoding
mkCharCE_ :: (CString) -> (CEType) -> IO ((SEXP V R.Char))
mkCharCE_ a1 a2 =
  let {a1' = id a1} in 
  let {a2' = cIntFromEnum a2} in 
  mkCharCE_'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 322 "src/Foreign/R.chs" #-}


mkCharCE :: CEType -> CString -> IO (SEXP V R.Char)
mkCharCE = flip mkCharCE_

-- | Intern a string @name@ into the symbol table.
--
-- If @name@ is not found, it is added to the symbol table. The symbol
-- corresponding to the string @name@ is returned.
install :: (CString) -> IO ((SEXP V R.Symbol))
install a1 =
  let {a1' = id a1} in 
  install'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 331 "src/Foreign/R.chs" #-}


-- | Allocate a 'SEXP'.
allocSEXP :: (SSEXPTYPE a) -> IO ((SEXP V a))
allocSEXP a1 =
  let {a1' = cUIntFromSingEnum a1} in 
  allocSEXP'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 335 "src/Foreign/R.chs" #-}


-- | Allocate a pairlist of 'SEXP's, chained together.
allocList :: (Int) -> IO ((SEXP V R.List))
allocList a1 =
  let {a1' = fromIntegral a1} in 
  allocList'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 338 "src/Foreign/R.chs" #-}


-- | Allocate Vector.
allocVector :: R.IsVector a => (SSEXPTYPE a) -> (Int) -> IO ((SEXP V a))
allocVector a1 a2 =
  let {a1' = cUIntFromSingEnum a1} in 
  let {a2' = fromIntegral a2} in 
  allocVector'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 343 "src/Foreign/R.chs" #-}


allocVectorProtected :: (R.IsVector a) => SSEXPTYPE a -> Int -> IO (SEXP s a)
allocVectorProtected ty n = fmap release (protect =<< allocVector ty n)

-- | Allocate a so-called cons cell, in essence a pair of 'SEXP' pointers.
cons :: (SEXP s a) -> (SEXP s b) -> IO ((SEXP V R.List))
cons a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  cons'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 349 "src/Foreign/R.chs" #-}


-- | Allocate a so-called cons cell of language objects, in essence a pair of
-- 'SEXP' pointers.
lcons :: (SEXP s a) -> (SEXP s b) -> IO ((SEXP V R.Lang))
lcons a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  lcons'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 353 "src/Foreign/R.chs" #-}


-- | Print a string representation of a 'SEXP' on the console.
printValue :: (SEXP s a) -> IO ()
printValue a1 =
  let {a1' = unsexp a1} in 
  printValue'_ a1' >>
  return ()

{-# LINE 356 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Garbage collection                                                         --
--------------------------------------------------------------------------------

-- | Protect a 'SEXP' from being garbage collected by R. It is in particular
-- necessary to do so for objects that are not yet pointed by any other object,
-- e.g. when constructing a tree bottom-up rather than top-down.
--
-- To avoid unbalancing calls to 'protect' and 'unprotect', do not use these
-- functions directly but use 'Language.R.withProtected' instead.
protect :: (SEXP s a) -> IO ((SEXP G a))
protect a1 =
  let {a1' = unsexp a1} in 
  protect'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 368 "src/Foreign/R.chs" #-}


-- | @unprotect n@ unprotects the last @n@ objects that were protected.
unprotect :: (Int) -> IO ()
unprotect a1 =
  let {a1' = fromIntegral a1} in 
  unprotect'_ a1' >>
  return ()

{-# LINE 371 "src/Foreign/R.chs" #-}


-- | Unprotect a specific object, referred to by pointer.
unprotectPtr :: (SEXP G a) -> IO ()
unprotectPtr a1 =
  let {a1' = unsexp a1} in 
  unprotectPtr'_ a1' >>
  return ()

{-# LINE 374 "src/Foreign/R.chs" #-}


-- | Invoke an R garbage collector sweep.
gc :: IO ()
gc =
  gc'_ >>
  return ()

{-# LINE 377 "src/Foreign/R.chs" #-}


-- | Preserve an object accross GCs.
preserveObject :: (SEXP s a) -> IO ()
preserveObject a1 =
  let {a1' = unsexp a1} in 
  preserveObject'_ a1' >>
  return ()

{-# LINE 380 "src/Foreign/R.chs" #-}


-- | Allow GC to remove an preserved object.
releaseObject :: (SEXP s a) -> IO ()
releaseObject a1 =
  let {a1' = unsexp a1} in 
  releaseObject'_ a1' >>
  return ()

{-# LINE 383 "src/Foreign/R.chs" #-}


--------------------------------------------------------------------------------
-- Evaluation                                                                 --
--------------------------------------------------------------------------------

-- | Evaluate any 'SEXP' to its value.
eval :: (SEXP s a) -> (SEXP s R.Env) -> IO ((SomeSEXP V))
eval a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  eval'_ a1' a2' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 391 "src/Foreign/R.chs" #-}


-- | Try to evaluate expression.
tryEval :: (SEXP s a) -> (SEXP s R.Env) -> (Ptr CInt) -> IO ((SomeSEXP V))
tryEval a1 a2 a3 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  let {a3' = id a3} in 
  tryEval'_ a1' a2' a3' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 395 "src/Foreign/R.chs" #-}


-- | Try to evaluate without printing error/warning messages to stdout.
tryEvalSilent :: (SEXP s a) -> (SEXP s R.Env) -> (Ptr CInt) -> IO ((SomeSEXP V))
tryEvalSilent a1 a2 a3 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  let {a3' = id a3} in 
  tryEvalSilent'_ a1' a2' a3' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 400 "src/Foreign/R.chs" #-}


-- | Construct a nullary function call.
lang1 :: (SEXP s a) -> IO ((SEXP V R.Lang))
lang1 a1 =
  let {a1' = unsexp a1} in 
  lang1'_ a1' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 403 "src/Foreign/R.chs" #-}


-- | Construct unary function call.
lang2 :: (SEXP s a) -> (SEXP s b) -> IO ((SEXP V R.Lang))
lang2 a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  lang2'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 406 "src/Foreign/R.chs" #-}


-- | Construct a binary function call.
lang3 :: (SEXP s a) -> (SEXP s b) -> (SEXP s c) -> IO ((SEXP V R.Lang))
lang3 a1 a2 a3 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  let {a3' = unsexp a3} in 
  lang3'_ a1' a2' a3' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 410 "src/Foreign/R.chs" #-}


-- | Find a function by name.
findFun :: (SEXP s a) -> (SEXP s R.Env) -> IO ((SomeSEXP s))
findFun a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  findFun'_ a1' a2' >>= \res ->
  let {res' = somesexp res} in
  return (res')

{-# LINE 414 "src/Foreign/R.chs" #-}


-- | Find a variable by name.
findVar :: (SEXP s a) -> (SEXP s R.Env) -> IO ((SEXP s R.Symbol))
findVar a1 a2 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  findVar'_ a1' a2' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 418 "src/Foreign/R.chs" #-}


mkWeakRef :: (SEXP s a) -> (SEXP s b) -> (SEXP s c) -> (Bool) -> IO ((SEXP V R.WeakRef))
mkWeakRef a1 a2 a3 a4 =
  let {a1' = unsexp a1} in 
  let {a2' = unsexp a2} in 
  let {a3' = unsexp a3} in 
  let {a4' = cIntFromEnum a4} in 
  mkWeakRef'_ a1' a2' a3' a4' >>= \res ->
  let {res' = sexp res} in
  return (res')

{-# LINE 421 "src/Foreign/R.chs" #-}


-------------------------------------------------------------------------------
-- Encoding                                                                  --
-------------------------------------------------------------------------------

-- | Content encoding.
data CEType = CE_NATIVE
            | CE_UTF8
            | CE_LATIN1
            | CE_BYTES
            | CE_SYMBOL
            | CE_ANY
  deriving (Eq,Show)
instance Enum CEType where
  succ CE_NATIVE = CE_UTF8
  succ CE_UTF8 = CE_LATIN1
  succ CE_LATIN1 = CE_BYTES
  succ CE_BYTES = CE_SYMBOL
  succ CE_SYMBOL = CE_ANY
  succ CE_ANY = error "CEType.succ: CE_ANY has no successor"

  pred CE_UTF8 = CE_NATIVE
  pred CE_LATIN1 = CE_UTF8
  pred CE_BYTES = CE_LATIN1
  pred CE_SYMBOL = CE_BYTES
  pred CE_ANY = CE_SYMBOL
  pred CE_NATIVE = error "CEType.pred: CE_NATIVE has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from CE_ANY

  fromEnum CE_NATIVE = 0
  fromEnum CE_UTF8 = 1
  fromEnum CE_LATIN1 = 2
  fromEnum CE_BYTES = 3
  fromEnum CE_SYMBOL = 5
  fromEnum CE_ANY = 99

  toEnum 0 = CE_NATIVE
  toEnum 1 = CE_UTF8
  toEnum 2 = CE_LATIN1
  toEnum 3 = CE_BYTES
  toEnum 5 = CE_SYMBOL
  toEnum 99 = CE_ANY
  toEnum unmatched = error ("CEType.toEnum: Cannot match " ++ show unmatched)

{-# LINE 428 "src/Foreign/R.chs" #-}


-- | Perform an action with resource while protecting it from the garbage
-- collection. This function is a safer alternative to 'R.protect' and
-- 'R.unprotect', guaranteeing that a protected resource gets unprotected
-- irrespective of the control flow, much like 'Control.Exception.bracket_'.
withProtected :: IO (SEXP V a)      -- Action to acquire resource
              -> (SEXP s a -> IO b) -- Action
              -> IO b
withProtected create f =
    bracket
      (do { x <- create; _ <- protect x; return x })
      (const $ unprotect 1)
      (f . unsafeRelease)

foreign import ccall safe "Foreign/R.chs.h CAR"
  car'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h CDR"
  cdr'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h TAG"
  tag'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h FRAME"
  envFrame'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h ENCLOS"
  envEnclosing'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h HASHTAB"
  envHashtab'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h FORMALS"
  closureFormals'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h BODY"
  closureBody'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h CLOENV"
  closureEnv'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h PRCODE"
  promiseCode'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h PRENV"
  promiseEnv'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h PRVALUE"
  promiseValue'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h TRUELENGTH"
  trueLength'_ :: ((SEXP0) -> (IO C2HSImp.CInt))

foreign import ccall safe "Foreign/R.chs.h R_CHAR"
  char'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr C2HSImp.CChar)))

foreign import ccall safe "Foreign/R.chs.h REAL"
  real'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr C2HSImp.CDouble)))

foreign import ccall unsafe "Foreign/R.chs.h INTEGER"
  integer'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr C2HSImp.CInt)))

foreign import ccall safe "Foreign/R.chs.h RAW"
  raw'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr C2HSImp.CUChar)))

foreign import ccall safe "Foreign/R.chs.h LOGICAL"
  logical'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr C2HSImp.CInt)))

foreign import ccall safe "Foreign/R.chs.h COMPLEX"
  complex'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr ())))

foreign import ccall safe "Foreign/R.chs.h STRING_PTR"
  string'_ :: ((SEXP0) -> (IO (C2HSImp.Ptr (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h VECTOR_ELT"
  readVector'_ :: ((SEXP0) -> (C2HSImp.CLong -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h SET_VECTOR_ELT"
  writeVector'_ :: ((SEXP0) -> (C2HSImp.CLong -> ((SEXP0) -> (IO (SEXP0)))))

foreign import ccall safe "Foreign/R.chs.h PRINTNAME"
  symbolPrintName'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h SYMVALUE"
  symbolValue'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h INTERNAL"
  symbolInternal'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_mkString"
  mkString'_ :: ((C2HSImp.Ptr C2HSImp.CChar) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_mkChar"
  mkChar'_ :: ((C2HSImp.Ptr C2HSImp.CChar) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_mkCharCE"
  mkCharCE_'_ :: ((C2HSImp.Ptr C2HSImp.CChar) -> (C2HSImp.CInt -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_install"
  install'_ :: ((C2HSImp.Ptr C2HSImp.CChar) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_allocSExp"
  allocSEXP'_ :: (C2HSImp.CUInt -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_allocList"
  allocList'_ :: (C2HSImp.CInt -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_allocVector"
  allocVector'_ :: (C2HSImp.CUInt -> (C2HSImp.CLong -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_cons"
  cons'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_lcons"
  lcons'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_PrintValue"
  printValue'_ :: ((SEXP0) -> (IO ()))

foreign import ccall safe "Foreign/R.chs.h Rf_protect"
  protect'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_unprotect"
  unprotect'_ :: (C2HSImp.CInt -> (IO ()))

foreign import ccall safe "Foreign/R.chs.h Rf_unprotect_ptr"
  unprotectPtr'_ :: ((SEXP0) -> (IO ()))

foreign import ccall safe "Foreign/R.chs.h R_gc"
  gc'_ :: (IO ())

foreign import ccall safe "Foreign/R.chs.h R_PreserveObject"
  preserveObject'_ :: ((SEXP0) -> (IO ()))

foreign import ccall safe "Foreign/R.chs.h R_ReleaseObject"
  releaseObject'_ :: ((SEXP0) -> (IO ()))

foreign import ccall safe "Foreign/R.chs.h Rf_eval"
  eval'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h R_tryEval"
  tryEval'_ :: ((SEXP0) -> ((SEXP0) -> ((C2HSImp.Ptr C2HSImp.CInt) -> (IO (SEXP0)))))

foreign import ccall safe "Foreign/R.chs.h R_tryEvalSilent"
  tryEvalSilent'_ :: ((SEXP0) -> ((SEXP0) -> ((C2HSImp.Ptr C2HSImp.CInt) -> (IO (SEXP0)))))

foreign import ccall safe "Foreign/R.chs.h Rf_lang1"
  lang1'_ :: ((SEXP0) -> (IO (SEXP0)))

foreign import ccall safe "Foreign/R.chs.h Rf_lang2"
  lang2'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_lang3"
  lang3'_ :: ((SEXP0) -> ((SEXP0) -> ((SEXP0) -> (IO (SEXP0)))))

foreign import ccall safe "Foreign/R.chs.h Rf_findFun"
  findFun'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h Rf_findVar"
  findVar'_ :: ((SEXP0) -> ((SEXP0) -> (IO (SEXP0))))

foreign import ccall safe "Foreign/R.chs.h R_MakeWeakRef"
  mkWeakRef'_ :: ((SEXP0) -> ((SEXP0) -> ((SEXP0) -> (C2HSImp.CInt -> (IO (SEXP0))))))