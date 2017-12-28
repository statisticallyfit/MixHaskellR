{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_H (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,9,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/bin"
libdir     = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/lib/x86_64-linux-ghc-8.0.2/H-0.9.0.1"
dynlibdir  = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/H-0.9.0.1"
libexecdir = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/libexec"
sysconfdir = "/datascience/projects/statisticallyfit/github/HaskellRMix/HaskellR/.stack-work/install/x86_64-linux/lts-9.3/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "H_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "H_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "H_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "H_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "H_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "H_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
