module Paths_markdown_monparsing (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/geo2a/.cabal/bin"
libdir     = "/home/geo2a/.cabal/lib/markdown-monparsing-0.1.0.0/ghc-7.6.3"
datadir    = "/home/geo2a/.cabal/share/markdown-monparsing-0.1.0.0"
libexecdir = "/home/geo2a/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "markdown_monparsing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "markdown_monparsing_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "markdown_monparsing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "markdown_monparsing_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
