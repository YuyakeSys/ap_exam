{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_aprel (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\bin"
libdir     = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\lib\\x86_64-windows-ghc-9.0.2\\aprel-0.0.0-5XBhCmmP7qZCqkPJnNHktl-aprel"
dynlibdir  = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\share\\x86_64-windows-ghc-9.0.2\\aprel-0.0.0"
libexecdir = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\libexec\\x86_64-windows-ghc-9.0.2\\aprel-0.0.0"
sysconfdir = "G:\\Advanced Programming\\reexam\\submit\\code\\aprel\\.stack-work\\install\\7e3d8866\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aprel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aprel_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aprel_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aprel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aprel_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aprel_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
