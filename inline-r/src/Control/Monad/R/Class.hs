-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.R.Class
  ( MonadR(..)
  , Region
  , acquireSome
  ) where

import Control.Memory.Region
import Foreign.R

import Control.Applicative
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Primitive (PrimMonad, PrimState)
import Prelude

-- | The class of R interaction monads. For safety, in compiled code we normally
-- use the 'Language.R.Instance.R' monad. For convenience, in a GHCi session, we
-- normally use the 'IO' monad directly (by means of a 'MonadR' instance for
-- 'IO', imported only in GHCi).
class (Applicative m, MonadIO m, MonadCatch m, MonadMask m, PrimMonad m)
      => MonadR m where
  -- | Lift an 'IO' action.
  io :: IO a -> m a
  io = liftIO

  -- | Acquire ownership in the current region of the given object. This means
  -- that the liveness of the object is guaranteed so long as the current region
  -- remains active (the R garbage collector will not attempt to free it).
  acquire :: s ~ V => SEXP s a -> m (SEXP (Region m) a)
  default acquire :: (MonadIO m, Region m ~ G) => SEXP s a -> m (SEXP (Region m) a)
  acquire = liftIO . protect

  -- | A reification of an R execution context, i.e. a "session".
  data ExecContext m :: *

  -- | Get the current execution context.
  getExecContext :: m (ExecContext m)

  -- | Provides no static guarantees that resources do not extrude the scope of
  -- their region. Acquired resources are not freed automatically upon exit.
  -- For internal use only.
  unsafeRunWithExecContext :: m a -> ExecContext m -> IO a

type Region m = PrimState m

-- | 'acquire' for 'SomeSEXP'.
acquireSome :: (MonadR m) => SomeSEXP V -> m (SomeSEXP (Region m))
acquireSome (SomeSEXP s) = SomeSEXP <$> acquire s
