module Paths_IRCbot (
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

bindir     = "/home/ysahil/.cabal/bin"
libdir     = "/home/ysahil/.cabal/lib/x86_64-linux-ghc-7.10.3/IRCbot-0.1.0.0-Gcs6JGkENvG6SsDLr1XhkX"
datadir    = "/home/ysahil/.cabal/share/x86_64-linux-ghc-7.10.3/IRCbot-0.1.0.0"
libexecdir = "/home/ysahil/.cabal/libexec"
sysconfdir = "/home/ysahil/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "IRCbot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "IRCbot_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "IRCbot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "IRCbot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "IRCbot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
