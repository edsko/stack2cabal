module Stack2Cabal.Options (
    Options(..)
  , getOptions
  ) where

import Options.Applicative

{-------------------------------------------------------------------------------
  Command line options
-------------------------------------------------------------------------------}

data Options = Options {
      -- | Path to the stack .yaml file
      stackYamlPath :: FilePath

      -- | Local directory for remote packages
    , remotePkgsDir :: FilePath

      -- | Dump parsed yaml
    , dumpParsedYaml :: Bool

      -- | Checkout remote packages to the specified local dir
    , checkoutRemotePkgs :: Bool

      -- | Write out cabal.project file
    , writeCabalProject :: Bool
    }

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
    <$> argument str (metavar "FILE")
    <*> (option str $ mconcat [
             long "deps"
           , help "Local directory to store remote packages"
           , value "stack2cabal-deps"
           , showDefault
           , metavar "DIR"
           ])
    <*> (switch $ mconcat [
             long "dump-parsed-yaml"
           , help "Dump parsed YAML"
           ])
    <*> (switch $ mconcat [
             long "checkout-remote-pkgs"
           , short 'c'
           , help "Checkout remote package"
           ])
    <*> (switch $ mconcat [
             long "write-cabal-project"
           , short 'w'
           , help "Write cabal.project file"
           ])

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
          fullDesc
        , progDesc "Convert Stack .yaml files to cabal .project files"
        ]
