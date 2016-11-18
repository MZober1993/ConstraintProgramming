module Paths_ConstraintProgramming (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/matthias/.cabal/bin"
libdir     = "/home/matthias/.cabal/lib/ConstraintProgramming-1.0/ghc-7.6.3"
datadir    = "/home/matthias/.cabal/share/ConstraintProgramming-1.0"
libexecdir = "/home/matthias/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ConstraintProgramming_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ConstraintProgramming_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ConstraintProgramming_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ConstraintProgramming_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
