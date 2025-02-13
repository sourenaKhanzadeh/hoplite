{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hoplite (
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
bindir     = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/bin"
libdir     = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/lib/x86_64-linux-ghc-9.2.8/hoplite-0.1.0.0-4TYCF4XKivp4u502Q6RTR5-hoplite"
dynlibdir  = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/lib/x86_64-linux-ghc-9.2.8"
datadir    = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/share/x86_64-linux-ghc-9.2.8/hoplite-0.1.0.0"
libexecdir = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/libexec/x86_64-linux-ghc-9.2.8/hoplite-0.1.0.0"
sysconfdir = "/home/suren/programming/hoplite/haskell/.stack-work/install/x86_64-linux/0d2c36af0fa5d57475422b693e97dde55a3cb991b1f15d76193b4ec12e810d65/9.2.8/etc"

getBinDir     = catchIO (getEnv "hoplite_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hoplite_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hoplite_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hoplite_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hoplite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hoplite_sysconfdir") (\_ -> return sysconfdir)




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
