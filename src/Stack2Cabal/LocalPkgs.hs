module Stack2Cabal.LocalPkgs (
    findOrMakeCabalFile
  ) where

import Control.Exception
import Data.List
import qualified Hpack as H
import System.Directory
import System.FilePath

import Stack2Cabal.StackYaml

{-------------------------------------------------------------------------------
  Local packages
-------------------------------------------------------------------------------}

-- | Return path of the cabal file of a local package. Stack supports hpack so
-- we need to handle `package.yml` files as well. For those run hpack and return
-- generated cabal file.
findOrMakeCabalFile :: LocalPackage -> IO FilePath
findOrMakeCabalFile dir = do
    files <- getDirectoryContents dir
    case filter isCabalFile files of
      [] | elem "package.yaml" files
           -> buildHpack
         | otherwise
           -> throwIO (userError notFound)
      [fp] -> return (dir </> fp)
      _    -> throwIO (userError multipleFound)
  where
    buildHpack = do
      let opts = H.setTarget (dir </> "package.yaml") H.defaultOptions
      result <- H.hpackResult opts
      return (H.resultCabalFile result)

    isCabalFile   = isSuffixOf ".cabal"
    notFound      = "No .cabal or package.yaml files found in " ++ dir
    multipleFound = "Multiple .cabal files found in " ++ dir
