module Paths_HCL (
  version,
  getBinDir, getLibDir, getDataDir, getLibexecDir,
  getDataFileName
  ) where

import Data.Version

version = makeVersion [1,0]

bindir     = "C:\\Program Files\\Haskell\\bin"
libdir     = "C:\\Program Files\\Haskell\\HCL-1.0\\ghc-6.6"
datadir    = "C:\\Program Files\\Common Files\\HCL-1.0"
libexecdir = "C:\\Program Files\\HCL-1.0"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "\\" ++ name)
