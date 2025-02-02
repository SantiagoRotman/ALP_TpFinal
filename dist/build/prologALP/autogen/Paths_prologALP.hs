{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_prologALP (
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
version = Version [0,1,0,2024] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/santi/.cabal/bin"
libdir     = "/home/santi/.cabal/lib/x86_64-linux-ghc-8.6.5/prologALP-0.1.0.2024-1T7yxsNwmyjJ5iEqX3lnHB-prologALP"
dynlibdir  = "/home/santi/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/santi/.cabal/share/x86_64-linux-ghc-8.6.5/prologALP-0.1.0.2024"
libexecdir = "/home/santi/.cabal/libexec/x86_64-linux-ghc-8.6.5/prologALP-0.1.0.2024"
sysconfdir = "/home/santi/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prologALP_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prologALP_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "prologALP_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "prologALP_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prologALP_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prologALP_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
