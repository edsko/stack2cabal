module Stack2Cabal.ExtraDeps (
    findExtraDeps
  ) where

import System.FilePath

import Stack2Cabal.StackYaml
import Stack2Cabal.RemotePkgs

-- | Find all extra dependencies
--
-- Assumes any remote packages have already been cloned.
findExtraDeps :: FilePath -> ParsedYaml -> IO [PackageName]
findExtraDeps remPkgsDir ParsedYaml{..} = do
    let extraDeps = map depPackageName stackExtraDeps
    remoteDeps <- concat <$>
                    mapM (remoteExtraDeps remPkgsDir) stackRemotePackages
    return (remoteDeps ++ extraDeps)

remoteExtraDeps :: FilePath -> RemotePackage -> IO [PackageName]
remoteExtraDeps remPkgsDir remPkg = do
    cabalFiles <- remotePkgCabalFiles remPkgsDir remPkg
    return $ map pkgNameFromCabalFile cabalFiles
  where
    pkgNameFromCabalFile :: FilePath -> PackageName
    pkgNameFromCabalFile = takeBaseName
