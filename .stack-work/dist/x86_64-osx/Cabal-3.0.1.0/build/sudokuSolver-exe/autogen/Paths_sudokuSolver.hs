{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudokuSolver (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/bin"
libdir     = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/lib/x86_64-osx-ghc-8.8.3/sudokuSolver-0.1.0.0-6HXyjxPbeC9KDepVuUNhjD-sudokuSolver-exe"
dynlibdir  = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/share/x86_64-osx-ghc-8.8.3/sudokuSolver-0.1.0.0"
libexecdir = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/libexec/x86_64-osx-ghc-8.8.3/sudokuSolver-0.1.0.0"
sysconfdir = "/Users/shriman/Documents/GitHub/sudokuSoluver/.stack-work/install/x86_64-osx/55a6ce22fd3713c2ce4c5c679e7232ba2709eab24b4e10984620fa26ecdf9d02/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudokuSolver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudokuSolver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudokuSolver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudokuSolver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudokuSolver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudokuSolver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
