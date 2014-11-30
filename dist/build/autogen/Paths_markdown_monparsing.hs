module Paths_markdown_monparsing (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/geo2a/Workspace/mmcs/bachelor/2014/markdown_monparsing/.cabal-sandbox/bin"
libdir     = "/home/geo2a/Workspace/mmcs/bachelor/2014/markdown_monparsing/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/markdown-monparsing-0.1.0.0"
datadir    = "/home/geo2a/Workspace/mmcs/bachelor/2014/markdown_monparsing/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/markdown-monparsing-0.1.0.0"
libexecdir = "/home/geo2a/Workspace/mmcs/bachelor/2014/markdown_monparsing/.cabal-sandbox/libexec"
sysconfdir = "/home/geo2a/Workspace/mmcs/bachelor/2014/markdown_monparsing/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "markdown_monparsing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "markdown_monparsing_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "markdown_monparsing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "markdown_monparsing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "markdown_monparsing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
