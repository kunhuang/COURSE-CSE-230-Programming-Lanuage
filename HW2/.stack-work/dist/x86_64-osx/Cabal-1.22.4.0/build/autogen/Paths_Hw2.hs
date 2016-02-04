module Paths_Hw2 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Kuhn/Dropbox/Study/UCSD 2015 Fall/CSE 230 Programming Language/HW/Hw2/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/bin"
libdir     = "/Users/Kuhn/Dropbox/Study/UCSD 2015 Fall/CSE 230 Programming Language/HW/Hw2/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/lib/x86_64-osx-ghc-7.10.2/Hw2-1.0-2vU6yhTiA8DHLkm2WjVPHM"
datadir    = "/Users/Kuhn/Dropbox/Study/UCSD 2015 Fall/CSE 230 Programming Language/HW/Hw2/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/share/x86_64-osx-ghc-7.10.2/Hw2-1.0"
libexecdir = "/Users/Kuhn/.cabal/libexec"
sysconfdir = "/Users/Kuhn/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hw2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hw2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hw2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hw2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hw2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
