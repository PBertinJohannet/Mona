{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mona (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/bin"
libdir     = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/lib/x86_64-linux-ghc-8.6.5/mona-0.1.0.0-34uBPASAf7880MZbPvTJRK-tests"
dynlibdir  = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/share/x86_64-linux-ghc-8.6.5/mona-0.1.0.0"
libexecdir = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/libexec/x86_64-linux-ghc-8.6.5/mona-0.1.0.0"
sysconfdir = "/home/pierre/Documents/prog/haskell/mona/.stack-work/install/x86_64-linux/22d1663323c2f81b03129f456184811ce7f315e9954f35740d6baac7833422b0/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mona_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mona_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mona_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mona_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mona_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mona_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
