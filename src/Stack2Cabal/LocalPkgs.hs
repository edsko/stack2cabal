module Stack2Cabal.LocalPkgs (
    findCabalFile
  ) where

import Control.Exception
import Data.List
import System.Directory
import System.FilePath

import Stack2Cabal.StackYaml

{-------------------------------------------------------------------------------
  Local packages
-------------------------------------------------------------------------------}

findCabalFile :: LocalPackage -> IO FilePath
findCabalFile dir = do
    files <- getDirectoryContents dir
    case filter isCabalFile files of
      []   -> throwIO (userError notFound)
      [fp] -> return (dir </> fp)
      _    -> throwIO (userError multipleFound)
  where
    isCabalFile   = isSuffixOf ".cabal"
    notFound      = "No .cabal file found in " ++ dir
    multipleFound = "Multiple .cabal files found in " ++ dir
