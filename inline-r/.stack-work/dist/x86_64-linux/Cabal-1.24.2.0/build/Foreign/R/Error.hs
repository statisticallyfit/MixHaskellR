-- GENERATED by C->Haskell Compiler, version 0.28.2 Switcheroo, 1 April 2016 (Haskell)
-- Edit the ORIGNAL .chs file instead!


{-# LINE 1 "src/Foreign/R/Error.chs" #-}
-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Exception type wrapping errors thrown by the R runtime.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.R.Error
  ( RError(..)
  ) where



import Control.Exception
import Data.Typeable

data RError = RError String
      deriving ( Typeable )

instance Show RError where
  show (RError s)      = "R Runtime Error: " ++ s

instance Exception RError