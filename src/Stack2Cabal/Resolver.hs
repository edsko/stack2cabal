module Stack2Cabal.Resolver (
    downloadCabalConfig
  ) where

import Control.Exception
import Control.Monad
import Data.List
import Network.HTTP.Simple

import qualified Data.ByteString.Lazy.UTF8 as LBS

import Stack2Cabal.StackYaml

{-------------------------------------------------------------------------------
  Download the cabal constraints for this version of the LTS
-------------------------------------------------------------------------------}

downloadCabalConfig :: Resolver -> IO [Dependency]
downloadCabalConfig resolver = do
    request <- parseRequest $ intercalate "/" [
        "https://www.stackage.org"
      , resolver
      , "cabal.config"
      ]
    response <- httpLBS request
    unless (getResponseStatusCode response == 200) $
      throwIO $ userError ("Could not download resolver cabal.config")
    return $ parseConstraints (LBS.toString (getResponseBody response))

parseConstraints :: String -> [Dependency]
parseConstraints cabalConfig =
    go "" (words cabalConfig)
  where
    go :: String -> [String] -> [Dependency]
    go pkg (('=' : '=' : version) : rest) =
          Dependency pkg (stripFinalComma version)
        : go "" rest
    go _ (w:ws) = go w ws
    go _ []     = []

    stripFinalComma :: String -> String
    stripFinalComma = reverse . dropWhile (== ',') . reverse
