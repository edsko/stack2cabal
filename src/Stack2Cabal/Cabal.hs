module Stack2Cabal.Cabal (
    writeCabalProjectFile
  ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.IO
import Generics.SOP (NP(..))

import Stack2Cabal.ExtraDeps
import Stack2Cabal.LocalPkgs
import Stack2Cabal.RemotePkgs
import Stack2Cabal.Resolver
import Stack2Cabal.StackYaml
import qualified Stack2Cabal.Keyed as Keyed

writeCabalProjectFile :: FilePath -> ParsedYaml -> IO ()
writeCabalProjectFile remPkgsDir parsedYaml@ParsedYaml{..} = do
    alreadyExists <- doesFileExist "cabal.project"
    when alreadyExists $
      throwIO (userError "cabal.project already exists. Refusing to overwrite.")
    withFile "cabal.project" WriteMode $ \h -> do
      -- Packages
      hPutStrLn h "packages:"

      -- .. local packages
      hPutStrLn h "-- local packages"
      do cabalFiles <- mapM findOrMakeCabalFile stackLocalPackages
         forM_ (zip (True : repeat False) cabalFiles) $ \(isFirst, cabalFile) ->
           hPutStrLn h $ concat [
               if isFirst then "    "
                          else "  , "
             , cabalFile
             ]

      -- .. remote packages
      hPutStrLn h "-- remote packages"
      do forM_ stackRemotePackages $ \remPkg -> do
           cabalFiles <- remotePkgCabalFiles remPkgsDir remPkg
           forM_ cabalFiles $ \cabalFile ->
             hPutStrLn h $ ("  , " ++ cabalFile)

      hPutStrLn h ""

      -- Flags and options
      let combined = Keyed.zip $ stackFlags
                              :* stackGhcOptions
                              :* Nil
      forM_ (Keyed.toList combined) $ \(pkg, mFlags :* mGhcOptions :* Nil) -> do
        hPutStrLn h $ "package " ++ pkg

        -- .. flags
        forM_ mFlags $ \flags -> do
          hPutStr h $ "  flags: "
          forM_ (Keyed.toList flags) $ \(flag, value) ->
            hPutStr h $ concat [
                if value then "+" else "-"
              , flag
              , " "
              ]
          hPutStrLn h "\n"

        -- .. ghc options
        forM_ mGhcOptions $ \ghcOptions -> do
          hPutStrLn h $ "  ghc-options: " ++ ghcOptions

      -- Constraints
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
