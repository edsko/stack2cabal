module Stack2Cabal.RemotePkgs (
    checkoutRemotePackages
  , remotePkgCabalFiles
  ) where

import Control.Exception
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Stack2Cabal.StackYaml
import Stack2Cabal.LocalPkgs

checkoutRemotePackages :: FilePath -> [RemotePackage] -> IO ()
checkoutRemotePackages remPkgsDir pkgs = do
    -- For now, just fail if the directory already exists
    createDirectory remPkgsDir
    mapM_ go pkgs
  where
    go :: RemotePackage -> IO ()
    go remPkg@RemotePackage{..} = do
        clone remPkgLocation
        cabalFiles <- remotePkgCabalFiles remPkgsDir remPkg
        mapM_ putStrLn cabalFiles

    clone :: Location -> IO ()
    clone loc@Git{..} = do
        callProcessIn Nothing "git" [
            "clone"
          , gitRepo
          , repoDir
          ]
        callProcessIn (Just repoDir) "git" [
            "checkout"
          , "-b", "stack2cabal"
          , gitCommit
          ]
      where
        repoDir = locationLocalDir remPkgsDir loc

locationLocalDir :: FilePath -> Location -> FilePath
locationLocalDir remPkgsDir Git{..} = remPkgsDir </> takeBaseName gitRepo

remotePkgCabalFiles :: FilePath -> RemotePackage -> IO [FilePath]
remotePkgCabalFiles remPkgsDir RemotePackage{..} =
    mapM findCabalFile cabalDirs
  where
    repoDir   = locationLocalDir remPkgsDir remPkgLocation
    cabalDirs = case remPkgSubdirs of
                  Nothing      -> [repoDir]
                  Just subdirs -> map (repoDir </>) subdirs

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Like 'callProcess', but allow to specify current working directory
--
-- We redirect standard output and error to a log file
callProcessIn :: Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessIn cwd prog args =
    withFile "stack2cabal.log" AppendMode $ \hOut -> do
      let process = (proc prog args) {
              cwd     = cwd
            , std_out = UseHandle hOut
            , std_err = UseHandle hOut
            }
      (_pIn, _pOut, _pErr, ph) <- createProcess process
      exitCode <- waitForProcess ph
      case exitCode of
        ExitSuccess      -> return ()
        ExitFailure code -> throwIO (userError (failed code))
  where
    failed :: Int -> String
    failed code = intercalate " " [
          prog
        , intercalate " " args
        , "failed with error code"
        , show code
        ]
