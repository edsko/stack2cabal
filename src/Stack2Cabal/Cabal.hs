module Stack2Cabal.Cabal (
    writeCabalProjectFile
  ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.IO

import Stack2Cabal.ExtraDeps
import Stack2Cabal.LocalPkgs
import Stack2Cabal.RemotePkgs
import Stack2Cabal.Resolver
import Stack2Cabal.StackYaml

writeCabalProjectFile :: FilePath -> ParsedYaml -> IO ()
writeCabalProjectFile remPkgsDir parsedYaml@ParsedYaml{..} = do
    alreadyExists <- doesFileExist "cabal.project"
    when alreadyExists $
      throwIO (userError "cabal.project already exists. Refusing to overwrite.")
    withFile "cabal.project" WriteMode $ \h -> do
      -- 1) Packages
      hPutStrLn h "packages:"

      -- 1a) Local packages
      hPutStrLn h "-- local packages"
      do cabalFiles <- mapM findCabalFile stackLocalPackages
         forM_ (zip (True : repeat False) cabalFiles) $ \(isFirst, cabalFile) ->
           hPutStrLn h $ concat [
               if isFirst then "    "
                          else "  , "
             , cabalFile
             ]

      -- 1b) Remote packages
      hPutStrLn h "-- remote packages"
      do forM_ stackRemotePackages $ \remPkg -> do
           cabalFiles <- remotePkgCabalFiles remPkgsDir remPkg
           forM_ cabalFiles $ \cabalFile ->
             hPutStrLn h $ ("  , " ++ cabalFile)

      hPutStrLn h ""

      -- 2) Flags TODO
      forM_ (getKeyed stackFlags) $ \(pkg, flags) -> do
        hPutStrLn h $ "package " ++ pkg
        hPutStr   h $ "  flags: "
        forM_ (getKeyed flags) $ \(flag, value) ->
          hPutStr h $ concat [
              if value then "+" else "-"
            , flag
            , " "
            ]
        hPutStrLn h "\n"

      -- 3) Constraints
      allConstraints <- downloadCabalConfig stackResolver
      extraDeps      <- findExtraDeps remPkgsDir parsedYaml
      let constraints = filter ((`notElem` extraDeps) . depPackageName)
                      $ allConstraints
      hPutStrLn h "constraints:"
      hPutStrLn h $ "-- " ++ stackResolver ++ " (except for extra dependencies)"
      forM_ (zip (True : repeat False) constraints) $ \(isFirst, dep) ->
        hPutStrLn h $ concat [
            if isFirst then "    "
                       else "  , "
          , depPackageName dep
          , " == "
          , depVersion dep
          ]
      hPutStrLn h $ "-- extra dependencies"
      forM_ stackExtraDeps $ \dep ->
        hPutStrLn h $ concat [
            "  , "
          , depPackageName dep
          , " == "
          , depVersion dep
          ]
      hPutStrLn h ""
