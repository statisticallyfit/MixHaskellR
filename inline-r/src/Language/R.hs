-- |
-- Copyright: 2013 (C) Amgen, Inc
--
-- Wrappers for low-level R functions.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# Language GADTs #-}
{-# Language ViewPatterns #-}

module Language.R
  ( module Foreign.R
  , module Foreign.R.Type
  , module Language.R.Instance
  , module Language.R.Globals
  , module Language.R.GC
  , module Language.R.Literal
  -- * Evaluation
  , eval
  , eval_
  , evalEnv
  , install
  , cancel
  -- * Exceptions
  , throwR
  , throwRMessage
  -- * Deprecated
  , parseFile
  , parseText
  , string
  , strings
  ) where

import           Control.Memory.Region
import qualified Data.Vector.SEXP as Vector
import Control.Monad.R.Class
import Foreign.R
  ( SEXP
  , SomeSEXP(..)
  , typeOf
  , asTypeOf
  , cast
  , unSomeSEXP
  , unsafeCoerce
  )
import qualified Foreign.R as R
import qualified Foreign.R.Parse as R
import qualified Foreign.R.Error as R
import           Foreign.R.Type
import           Language.R.GC
import           Language.R.Globals
import           Language.R.HExp
import           Language.R.Instance
import           {-# SOURCE #-} Language.R.Internal
import           Language.R.Literal

import Control.Applicative
import Control.Exception ( throwIO )
import Control.Monad ( (>=>), when, unless, forM, void )
import Data.ByteString as B
import Data.ByteString.Char8 as C8 ( pack, unpack )
import Data.Singletons (sing)
import Foreign
  ( alloca
  , castPtr
  , peek
  , poke
  )
import Foreign.C.String ( withCString, peekCString )
import Prelude

-- NOTE: In this module, cannot use quasiquotations, since we are lower down in
-- the dependency hierarchy.

-- | Parse and then evaluate expression.
parseEval :: ByteString -> IO (SomeSEXP V)
parseEval txt = useAsCString txt $ \ctxt ->
  R.withProtected (R.mkString ctxt) $ \rtxt ->
    alloca $ \status -> do
      R.withProtected (R.parseVector rtxt 1 status (R.release nilValue)) $ \exprs -> do
        rc <- fromIntegral <$> peek status
        unless (R.PARSE_OK == toEnum rc) $
          runRegion $ throwRMessage $ "Parse error in: " ++ C8.unpack txt
        SomeSEXP expr <- peek $ castPtr $ R.unsafeSEXPToVectorPtr exprs
        runRegion $ do
          SomeSEXP val <- eval expr
          return $ SomeSEXP (R.release val)

-- | Parse file and perform some actions on parsed file.
--
-- This function uses continuation because this is an easy way to make
-- operations GC-safe.
parseFile :: FilePath -> (SEXP s 'R.Expr -> IO a) -> IO a
{-# DEPRECATED parseFile "Use [r| parse(file=\"path/to/file\") |] instead." #-}
parseFile fl f = do
    withCString fl $ \cfl ->
      R.withProtected (R.mkString cfl) $ \rfl ->
        r1 (C8.pack "parse") rfl >>= \(R.SomeSEXP s) ->
          return (R.unsafeCoerce s) `R.withProtected` f

parseText
  :: String -- ^ Text to parse
  -> Bool   -- ^ Whether to annotate the AST with source locations.
  -> IO (R.SEXP V 'R.Expr)
{-# DEPRECATED parseText "Use [r| parse(text=...) |] instead." #-}
parseText txt b = do
    s <- parseEval $ C8.pack $
         "parse(text=" ++ show txt ++ ", keep.source=" ++ keep ++ ")"
    return $ (sing :: R.SSEXPTYPE 'R.Expr) `R.cast` s
  where
    keep | b         = "TRUE"
         | otherwise = "FALSE"

-- | Internalize a symbol name.
install :: MonadR m => String -> m (SEXP V 'R.Symbol)
install = io . installIO

{-# DEPRECATED string, strings "Use mkSEXP instead" #-}

-- | Create an R character string from a Haskell string.
string :: String -> IO (SEXP V 'R.Char)
string str = withCString str R.mkChar

-- | Create an R string vector from a Haskell string.
strings :: String -> IO (SEXP V 'R.String)
strings str = withCString str R.mkString

-- | Evaluate a (sequence of) expression(s) in the given environment, returning the
-- value of the last.
evalEnv :: MonadR m => SEXP s a -> SEXP s 'R.Env -> m (SomeSEXP (Region m))
evalEnv (hexp -> Language.R.HExp.Expr _ v) rho = acquireSome =<< do
    io $ alloca $ \p -> do
      mapM_ (\(SomeSEXP s) -> void $ R.protect s) (Vector.toList v)
      x <- Prelude.last <$> forM (Vector.toList v) (\(SomeSEXP s) -> do
          z <- R.tryEvalSilent s rho p
          e <- peek p
          when (e /= 0) $ runRegion $ throwR rho
          return z)
      R.unprotect (Vector.length v)
      return x
evalEnv x rho = acquireSome =<< do
    io $ alloca $ \p -> R.withProtected (return (R.release x)) $ \_ -> do
      v <- R.tryEvalSilent x rho p
      e <- peek p
      when (e /= 0) $ runRegion $ throwR rho
      return v

-- | Evaluate a (sequence of) expression(s) in the global environment.
eval :: MonadR m => SEXP s a -> m (SomeSEXP (Region m))
eval x = evalEnv x (R.release globalEnv)

-- | Silent version of 'eval' function that discards it's result.
eval_ :: MonadR m => SEXP s a -> m ()
eval_ = void . eval

-- | Throw an R error as an exception.
throwR :: MonadR m => R.SEXP s 'R.Env   -- ^ Environment in which to find error.
       -> m a
throwR env = getErrorMessage env >>= io . throwIO . R.RError

-- | Cancel any ongoing R computation in the current process. After interruption
-- an 'RError' exception will be raised.
--
-- This call is safe to run in any thread. If there is no R computation running,
-- the next computaion will be immediately cancelled. Note that R will only
-- interrupt computations at so-called "safe points" (in particular, not in the
-- middle of a C call).
cancel :: IO ()
cancel = poke R.interruptsPending 1

-- | Throw an R exception with specified message.
throwRMessage :: MonadR m => String -> m a
throwRMessage = io . throwIO . R.RError

-- | Read last error message.
getErrorMessage :: MonadR m => R.SEXP s 'R.Env -> m String
getErrorMessage e = io $ do
  R.withProtected (withCString "geterrmessage" ((R.install >=> R.lang1))) $ \f -> do
    R.withProtected (return (R.release e)) $ \env -> do
      peekCString
        =<< R.char
        =<< peek
        =<< R.string . R.cast (sing :: R.SSEXPTYPE 'R.String)
        =<< R.eval f env
