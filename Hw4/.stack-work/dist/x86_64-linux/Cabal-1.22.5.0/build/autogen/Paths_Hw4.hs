module Paths_Hw4 (
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

bindir     = "/home/cse230/Desktop/COURSE-CSE-230-Programming-Lanuage/Hw4/.stack-work/install/x86_64-linux/nightly-2016-01-06/7.10.3/bin"
libdir     = "/home/cse230/Desktop/COURSE-CSE-230-Programming-Lanuage/Hw4/.stack-work/install/x86_64-linux/nightly-2016-01-06/7.10.3/lib/x86_64-linux-ghc-7.10.3/Hw4-1.0-Lu6RtrOiWHuLZpns6goq7j"
datadir    = "/home/cse230/Desktop/COURSE-CSE-230-Programming-Lanuage/Hw4/.stack-work/install/x86_64-linux/nightly-2016-01-06/7.10.3/share/x86_64-linux-ghc-7.10.3/Hw4-1.0"
libexecdir = "/home/cse230/Desktop/COURSE-CSE-230-Programming-Lanuage/Hw4/.stack-work/install/x86_64-linux/nightly-2016-01-06/7.10.3/libexec"
sysconfdir = "/home/cse230/Desktop/COURSE-CSE-230-Programming-Lanuage/Hw4/.stack-work/install/x86_64-linux/nightly-2016-01-06/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hw4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hw4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hw4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hw4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hw4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
