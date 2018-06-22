module Main where

import Control.Monad

import Stack2Cabal.Options
import Stack2Cabal.RemotePkgs
import Stack2Cabal.StackYaml
import Stack2Cabal.Cabal

{-------------------------------------------------------------------------------
  Top-level main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    Options{..} <- getOptions
    parsed@StackYaml{..} <- readStackYaml stackYamlPath
    let ParsedYaml{..} = stackYamlParsed
    when dumpParsedYaml $
      print parsed
    when checkoutRemotePkgs $
      checkoutRemotePackages remotePkgsDir stackRemotePackages
    when writeCabalProject $
      writeCabalProjectFile remotePkgsDir stackYamlParsed
