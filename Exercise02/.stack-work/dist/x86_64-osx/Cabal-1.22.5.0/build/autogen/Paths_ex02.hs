module Paths_ex02 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/michaeloboyle/Documents/GitHub/functional-programming/Exercise02/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/bin"
libdir     = "/Users/michaeloboyle/Documents/GitHub/functional-programming/Exercise02/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/lib/x86_64-osx-ghc-7.10.3/ex02-0.1.0.0-FNe9vPG3iyEKTyBf8I5Adp"
datadir    = "/Users/michaeloboyle/Documents/GitHub/functional-programming/Exercise02/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/share/x86_64-osx-ghc-7.10.3/ex02-0.1.0.0"
libexecdir = "/Users/michaeloboyle/Documents/GitHub/functional-programming/Exercise02/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/libexec"
sysconfdir = "/Users/michaeloboyle/Documents/GitHub/functional-programming/Exercise02/.stack-work/install/x86_64-osx/lts-6.19/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ex02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ex02_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ex02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ex02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ex02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
