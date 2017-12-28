{-# LINE 1 "src/Foreign/R/Type.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "src/Foreign/R/Type.hsc" #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


{-# LINE 11 "src/Foreign/R/Type.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LINE 13 "src/Foreign/R/Type.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Definition of 'SEXPTYPE', which classifies the possible forms of an
-- R expression (a 'SEXP'). It is normally not necessary to import this module
-- directly, since it is reexported by "Foreign.R".
--
-- This is done in a separate module because we want to use hsc2hs rather than
-- c2hs for discharging the boilerplate around 'SEXPTYPE'. This is because
-- 'SEXPTYPE' is nearly but not quite a true enumeration and c2hs has trouble
-- dealing with that.
--
-- This module also defines a singleton version of 'SEXPTYPE', called
-- 'SSEXPTYPE'. This is actually a family of types, one for each possible
-- 'SEXPTYPE'. Singleton types are a way of emulating dependent types in
-- a language that does not have true dependent type. They are useful in
-- functions whose result type depends on the value of one of its arguments. See
-- e.g. 'Foreign.R.allocVector'.

module Foreign.R.Type
  ( SEXPTYPE(..)
  , SSEXPTYPE
  , Sing(..)
  , Logical(..)
  , PairList
  , IsVector
  , IsGenericVector
  , IsList
  , IsPairList
  , IsExpression
  ) where


{-# LINE 48 "src/Foreign/R/Type.hsc" #-}

import Foreign.R.Constraints
import Internal.Error

import qualified Language.Haskell.TH.Syntax as Hs
import qualified Language.Haskell.TH.Lib as Hs

import Data.Singletons.TH

import Control.DeepSeq (NFData(..))
import Foreign (castPtr)
import Foreign.C (CInt)
import Foreign.Storable(Storable(..))

-- | R \"type\". Note that what R calls a \"type\" is not what is usually meant
-- by the term: there is really only a single type, called 'SEXP', and an
-- R "type" in fact refers to the /class/ or /form/ of the expression.
--
-- To better illustrate the distinction, note that any sane type system normally
-- has the /subject reduction property/: that the type of an expression is
-- invariant under reduction. For example, @(\x -> x) 1@ has type 'Int', and so
-- does the value of this expression, @2@, have type 'Int'. Yet the /form/ of
-- the expression is an application of a function to a literal, while the form
-- of its reduct is an integer literal.
--
-- We introduce convenient Haskell-like names for forms because this datatype is
-- used to index 'SEXP' and other types through the @DataKinds@ extension.
--
data SEXPTYPE
    = Nil
    | Symbol
    | List
    | Closure
    | Env
    | Promise
    | Lang
    | Special
    | Builtin
    | Char
    | Logical
    | Int
    | Real
    | Complex
    | String
    | DotDotDot
    | Any
    | Vector
    | Expr
    | Bytecode
    | ExtPtr
    | WeakRef
    | Raw
    | S4
    | New
    | Free
    | Fun
    deriving (Eq, Ord, Show)

instance Enum SEXPTYPE where
  fromEnum Nil        = 0
{-# LINE 108 "src/Foreign/R/Type.hsc" #-}
  fromEnum Symbol     = 1
{-# LINE 109 "src/Foreign/R/Type.hsc" #-}
  fromEnum List       = 2
{-# LINE 110 "src/Foreign/R/Type.hsc" #-}
  fromEnum Closure    = 3
{-# LINE 111 "src/Foreign/R/Type.hsc" #-}
  fromEnum Env        = 4
{-# LINE 112 "src/Foreign/R/Type.hsc" #-}
  fromEnum Promise    = 5
{-# LINE 113 "src/Foreign/R/Type.hsc" #-}
  fromEnum Lang       = 6
{-# LINE 114 "src/Foreign/R/Type.hsc" #-}
  fromEnum Special    = 7
{-# LINE 115 "src/Foreign/R/Type.hsc" #-}
  fromEnum Builtin    = 8
{-# LINE 116 "src/Foreign/R/Type.hsc" #-}
  fromEnum Char       = 9
{-# LINE 117 "src/Foreign/R/Type.hsc" #-}
  fromEnum Logical    = 10
{-# LINE 118 "src/Foreign/R/Type.hsc" #-}
  fromEnum Int        = 13
{-# LINE 119 "src/Foreign/R/Type.hsc" #-}
  fromEnum Real       = 14
{-# LINE 120 "src/Foreign/R/Type.hsc" #-}
  fromEnum Complex    = 15
{-# LINE 121 "src/Foreign/R/Type.hsc" #-}
  fromEnum String     = 16
{-# LINE 122 "src/Foreign/R/Type.hsc" #-}
  fromEnum DotDotDot  = 17
{-# LINE 123 "src/Foreign/R/Type.hsc" #-}
  fromEnum Any        = 18
{-# LINE 124 "src/Foreign/R/Type.hsc" #-}
  fromEnum Vector     = 19
{-# LINE 125 "src/Foreign/R/Type.hsc" #-}
  fromEnum Expr       = 20
{-# LINE 126 "src/Foreign/R/Type.hsc" #-}
  fromEnum Bytecode   = 21
{-# LINE 127 "src/Foreign/R/Type.hsc" #-}
  fromEnum ExtPtr     = 22
{-# LINE 128 "src/Foreign/R/Type.hsc" #-}
  fromEnum WeakRef    = 23
{-# LINE 129 "src/Foreign/R/Type.hsc" #-}
  fromEnum Raw        = 24
{-# LINE 130 "src/Foreign/R/Type.hsc" #-}
  fromEnum S4         = 25
{-# LINE 131 "src/Foreign/R/Type.hsc" #-}
  fromEnum New        = 30
{-# LINE 132 "src/Foreign/R/Type.hsc" #-}
  fromEnum Free       = 31
{-# LINE 133 "src/Foreign/R/Type.hsc" #-}
  fromEnum Fun        = 99
{-# LINE 134 "src/Foreign/R/Type.hsc" #-}

  toEnum (0)     = Nil
{-# LINE 136 "src/Foreign/R/Type.hsc" #-}
  toEnum (1)     = Symbol
{-# LINE 137 "src/Foreign/R/Type.hsc" #-}
  toEnum (2)    = List
{-# LINE 138 "src/Foreign/R/Type.hsc" #-}
  toEnum (3)     = Closure
{-# LINE 139 "src/Foreign/R/Type.hsc" #-}
  toEnum (4)     = Env
{-# LINE 140 "src/Foreign/R/Type.hsc" #-}
  toEnum (5)    = Promise
{-# LINE 141 "src/Foreign/R/Type.hsc" #-}
  toEnum (6)    = Lang
{-# LINE 142 "src/Foreign/R/Type.hsc" #-}
  toEnum (7) = Special
{-# LINE 143 "src/Foreign/R/Type.hsc" #-}
  toEnum (8) = Builtin
{-# LINE 144 "src/Foreign/R/Type.hsc" #-}
  toEnum (9)    = Char
{-# LINE 145 "src/Foreign/R/Type.hsc" #-}
  toEnum (10)     = Logical
{-# LINE 146 "src/Foreign/R/Type.hsc" #-}
  toEnum (13)     = Int
{-# LINE 147 "src/Foreign/R/Type.hsc" #-}
  toEnum (14)    = Real
{-# LINE 148 "src/Foreign/R/Type.hsc" #-}
  toEnum (15)    = Complex
{-# LINE 149 "src/Foreign/R/Type.hsc" #-}
  toEnum (16)     = String
{-# LINE 150 "src/Foreign/R/Type.hsc" #-}
  toEnum (17)     = DotDotDot
{-# LINE 151 "src/Foreign/R/Type.hsc" #-}
  toEnum (18)     = Any
{-# LINE 152 "src/Foreign/R/Type.hsc" #-}
  toEnum (19)     = Vector
{-# LINE 153 "src/Foreign/R/Type.hsc" #-}
  toEnum (20)    = Expr
{-# LINE 154 "src/Foreign/R/Type.hsc" #-}
  toEnum (21)   = Bytecode
{-# LINE 155 "src/Foreign/R/Type.hsc" #-}
  toEnum (22)  = ExtPtr
{-# LINE 156 "src/Foreign/R/Type.hsc" #-}
  toEnum (23) = WeakRef
{-# LINE 157 "src/Foreign/R/Type.hsc" #-}
  toEnum (24)     = Raw
{-# LINE 158 "src/Foreign/R/Type.hsc" #-}
  toEnum (25)      = S4
{-# LINE 159 "src/Foreign/R/Type.hsc" #-}
  toEnum (30)     = New
{-# LINE 160 "src/Foreign/R/Type.hsc" #-}
  toEnum (31)    = Free
{-# LINE 161 "src/Foreign/R/Type.hsc" #-}
  toEnum (99)     = Fun
{-# LINE 162 "src/Foreign/R/Type.hsc" #-}
  toEnum _                   = violation "toEnum" "Unknown R type."

instance NFData SEXPTYPE where
  rnf = (`seq` ())

genSingletons [''SEXPTYPE]

instance Hs.Lift SEXPTYPE where
  lift a = [| $(Hs.conE (Hs.mkName $ "Foreign.R.Type." ++ show a)) |]

-- | R uses three-valued logic.
data Logical = FALSE
             | TRUE
             | NA
-- XXX no Enum instance because NA = INT_MIN, not representable as an Int on
-- 32-bit systems.
               deriving (Eq, Ord, Show)

instance Storable Logical where
  sizeOf _       = sizeOf (undefined :: CInt)
  alignment _    = alignment (undefined :: CInt)
  poke ptr FALSE = poke (castPtr ptr) (0 :: CInt)
  poke ptr TRUE  = poke (castPtr ptr) (1 :: CInt)
  -- Currently NA_LOGICAL = INT_MIN.
  poke ptr NA    = poke (castPtr ptr) (-2147483648 :: CInt)
{-# LINE 187 "src/Foreign/R/Type.hsc" #-}
  peek ptr = do
      x <- peek (castPtr ptr)
      case x :: CInt of
          0 -> return FALSE
          1 -> return TRUE
          -2147483648 -> return NA
{-# LINE 193 "src/Foreign/R/Type.hsc" #-}
          _ -> failure "Storable Logical peek" "Not a Logical."

-- | Used where the R documentation speaks of "pairlists", which are really just
-- regular lists.
type PairList = List

-- Use a macro to avoid having to define append at the type level.

{-# LINE 210 "src/Foreign/R/Type.hsc" #-}

-- | Constraint synonym grouping all vector forms into one class. @IsVector a@
-- holds iff R's @is.vector()@ returns @TRUE@.
type IsVector (a :: SEXPTYPE) = (SingI a, a :∈  'Char                    ': 'Logical                    ': 'Int                    ': 'Real                    ': 'Complex                    ': 'String                    ': 'Vector                    ': 'Expr                    ': 'WeakRef                    ': 'Raw ': '[])
{-# LINE 214 "src/Foreign/R/Type.hsc" #-}

-- | Non-atomic vector forms. See @src\/main\/memory.c:SET_VECTOR_ELT@ in the
-- R source distribution.
type IsGenericVector (a :: SEXPTYPE) = (SingI a, a :∈ [Vector, Expr, WeakRef])

-- | @IsList a@ holds iff R's @is.list()@ returns @TRUE@.
type IsList (a :: SEXPTYPE) = (SingI a, a :∈  'Char                    ': 'Logical                    ': 'Int                    ': 'Real                    ': 'Complex                    ': 'String                    ': 'Vector                    ': 'Expr                    ': 'WeakRef                    ': 'Raw ': List ': '[])
{-# LINE 221 "src/Foreign/R/Type.hsc" #-}

-- | @IsPairList a@ holds iff R's @is.pairlist()@ returns @TRUE@.
type IsPairList (a :: SEXPTYPE) = (SingI a, a :∈ [List, Nil])

-- | Constraint synonym grouping all expression forms into one class. According
-- to R internals, an expression is usually a 'Lang', but can sometimes also be
-- an 'Expr' or a 'Symbol'.
type IsExpression (a :: SEXPTYPE) = (SingI a, a :∈ [Lang, Expr, Symbol])
