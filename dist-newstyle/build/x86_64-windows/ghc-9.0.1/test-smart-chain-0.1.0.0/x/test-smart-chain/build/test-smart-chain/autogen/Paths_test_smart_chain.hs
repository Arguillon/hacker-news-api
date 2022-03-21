{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_test_smart_chain (
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

bindir     = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\test-smart-chain-0.1.0.0-inplace-test-smart-chain"
dynlibdir  = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\test-smart-chain-0.1.0.0"
libexecdir = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\test-smart-chain-0.1.0.0-inplace-test-smart-chain\\x86_64-windows-ghc-9.0.1\\test-smart-chain-0.1.0.0"
sysconfdir = "C:\\Users\\arthu\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "test_smart_chain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "test_smart_chain_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "test_smart_chain_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "test_smart_chain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "test_smart_chain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "test_smart_chain_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
