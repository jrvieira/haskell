{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sim (
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

bindir     = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/bin"
libdir     = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/lib/x86_64-linux-ghc-8.6.3/sim-0.1.0.0-9RTFSgrmfOXE40eP3uABpl-sim-test"
dynlibdir  = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/share/x86_64-linux-ghc-8.6.3/sim-0.1.0.0"
libexecdir = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/libexec/x86_64-linux-ghc-8.6.3/sim-0.1.0.0"
sysconfdir = "/home/zero/mac/jrvieira/coda/dojo/haskell/tapestry/.stack-work/install/x86_64-linux/lts-13.0/8.6.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sim_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sim_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sim_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sim_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sim_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sim_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
