module Paths_indices (
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
version = Version {versionBranch = [1,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mak/.cabal/bin"
libdir     = "/home/mak/.cabal/lib/x86_64-linux-ghc-7.7.20131205/indices-1.3"
datadir    = "/home/mak/.cabal/share/x86_64-linux-ghc-7.7.20131205/indices-1.3"
libexecdir = "/home/mak/.cabal/libexec"
sysconfdir = "/home/mak/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "indices_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "indices_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "indices_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "indices_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "indices_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
