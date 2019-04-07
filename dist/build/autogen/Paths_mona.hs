{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
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

bindir     = "/home/pierre/.cabal/bin"
libdir     = "/home/pierre/.cabal/lib/x86_64-linux-ghc-8.0.2/mona-0.1.0.0"
dynlibdir  = "/home/pierre/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/pierre/.cabal/share/x86_64-linux-ghc-8.0.2/mona-0.1.0.0"
libexecdir = "/home/pierre/.cabal/libexec"
sysconfdir = "/home/pierre/.cabal/etc"

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
