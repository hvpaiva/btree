{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_btree_algorithm (
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

bindir     = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/bin"
libdir     = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/lib/x86_64-osx-ghc-9.0.2/btree-algorithm-0.1.0.0-7L4JkDQcvekL6uwZ2p2b8d-btree-algorithm"
dynlibdir  = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/share/x86_64-osx-ghc-9.0.2/btree-algorithm-0.1.0.0"
libexecdir = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/libexec/x86_64-osx-ghc-9.0.2/btree-algorithm-0.1.0.0"
sysconfdir = "/Users/bi003188/dev/haskell/btree-algorithm/.stack-work/install/x86_64-osx/a59d35b1beba5ca83ab88166ed625e822d0e6feb50c8324a941295de85464806/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "btree_algorithm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "btree_algorithm_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "btree_algorithm_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "btree_algorithm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "btree_algorithm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "btree_algorithm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
