{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_minesweeper (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/bin"
libdir     = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/lib/aarch64-osx-ghc-9.6.6/minesweeper-0.1.0.0-IMjVFHyFyLc9qXF0MPUxxu-minesweeper"
dynlibdir  = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/share/aarch64-osx-ghc-9.6.6/minesweeper-0.1.0.0"
libexecdir = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/libexec/aarch64-osx-ghc-9.6.6/minesweeper-0.1.0.0"
sysconfdir = "/Users/serzhanator/Documents/repos/minesweeper/.stack-work/install/aarch64-osx/4044edbce833ada5a2a0433d8901933c92cba52988d772531c8f13a01d58ae2e/9.6.6/etc"

getBinDir     = catchIO (getEnv "minesweeper_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "minesweeper_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "minesweeper_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "minesweeper_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minesweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minesweeper_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
