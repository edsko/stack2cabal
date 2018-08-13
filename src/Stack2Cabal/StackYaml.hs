module Stack2Cabal.StackYaml (
    -- * Types
    Resolver
  , LocalPackage
  , PackageName
  , Version
  , StackYaml(..)
  , ParsedYaml(..)
  , RemotePackage(..)
  , Location(..)
  , Dependency(..)
    -- * Parsing
  , readStackYaml
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson.Types (typeMismatch)
import Data.Coerce (coerce)
import Data.Either
import Data.Foldable (asum)
import Data.List
import Data.Yaml

import qualified Data.Text as T

import Stack2Cabal.Util
import Stack2Cabal.Keyed (Keyed)
import qualified Stack2Cabal.Keyed as Keyed

{-------------------------------------------------------------------------------
  Representation of the stack .yaml file
-------------------------------------------------------------------------------}

type Resolver     = String
type LocalPackage = String
type PackageName  = String
type Version      = String

data StackYaml = StackYaml {
      stackYamlParsed :: ParsedYaml
    , stackYamlRaw    :: Value
    }
  deriving (Show)

data ParsedYaml = ParsedYaml {
      -- | Resolver version
      stackResolver :: Resolver

      -- | Local packages
    , stackLocalPackages :: [LocalPackage]

      -- | Remote packages
    , stackRemotePackages :: [RemotePackage]

      -- | Extra dependencies
    , stackExtraDeps :: [Dependency]

      -- | Flags (pkg -> flag -> bool)
    , stackFlags :: Keyed (Keyed Bool)

      -- | GHC options (pkg -> options)
    , stackGhcOptions :: Keyed String
    }
  deriving (Show)

data RemotePackage = RemotePackage {
      remPkgLocation :: Location
    , remPkgSubdirs  :: Maybe [String]
    , remPkgExtraDep :: Bool
    }
  deriving (Show)

data Location =
    Git {
        gitRepo   :: String
      , gitCommit :: String
      }
  deriving (Show)

data Dependency = Dependency {
      depPackageName :: PackageName
    , depVersion     :: Version
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Internal types
-------------------------------------------------------------------------------}

-- We split packages up before returning them
newtype Package = Package (Either String RemotePackage)

partitionPackages :: [Package] -> ([String], [RemotePackage])
partitionPackages = partitionEithers . coerce

-- Similarly, we also split dependencies into regular and remote
newtype DependencyOrRemote = DependencyOrRemote (Either Dependency LocationWithSubdirs)

partitionDependencies :: [DependencyOrRemote] -> ([Dependency], [RemotePackage])
partitionDependencies = fmap (map aux) . partitionEithers . coerce
  where
    aux :: LocationWithSubdirs -> RemotePackage
    aux (LocationWithSubdirs loc subdirs) = RemotePackage{
          remPkgLocation = loc
        , remPkgSubdirs  = subdirs
        , remPkgExtraDep = True
        }

-- For some reason stack allows subdirs as part of the location
data LocationWithSubdirs = LocationWithSubdirs Location (Maybe [String])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

instance FromJSON StackYaml where
  parseJSON = keepValue StackYaml $
      withObject "Stack yaml file" $ \obj -> do
        stackResolver   <- obj .:  "resolver"
        packages        <- obj .:  "packages"
        dependencies    <- obj .:? "extra-deps"  .!= []
        stackFlags      <- obj .:? "flags"       .!= Keyed.empty
        stackGhcOptions <- obj .:? "ghc-options" .!= Keyed.empty
        let (stackLocalPackages, remotePkgs) = partitionPackages     packages
            (stackExtraDeps,     remoteDeps) = partitionDependencies dependencies
            stackRemotePackages              = remotePkgs ++ remoteDeps
        return ParsedYaml{..}

instance FromJSON RemotePackage where
  parseJSON = withObject "remote package" $ \obj -> do
      LocationWithSubdirs remPkgLocation locSubdirs <- obj .: "location"
      remPkgSubdirs  <- (locSubdirs <|>) <$> obj .:? "subdirs"
      remPkgExtraDep <- obj .:? "extra-dep" .!= True
      return RemotePackage{..}

instance FromJSON Package where
  parseJSON = stringOr (either (Package . Left) (Package . Right)) $ parseJSON

instance FromJSON DependencyOrRemote where
  parseJSON val = asum [
      (DependencyOrRemote . Left)  <$> parseJSON val
    , (DependencyOrRemote . Right) <$> parseJSON val
    ]

instance FromJSON LocationWithSubdirs where
  parseJSON = withObject "location" $ \obj -> msum [
        do gitRepo   <- obj .:  "git"
           gitCommit <- obj .:  "commit"
           subdirs   <- obj .:? "subdirs"
           return (LocationWithSubdirs Git{..} subdirs)
      ]

instance FromJSON Dependency where
  parseJSON = withString "extra-deps" $ parseDep
    where
      parseDep :: String -> Parser Dependency
      parseDep str =
          case reverse (splitWhen (== '-') str) of
            [] -> fail $ "Could not parse " ++ show str
            version:pkgRev -> do
              let pkg = intercalate "-" (reverse pkgRev)
              return $ Dependency pkg version

{-------------------------------------------------------------------------------
  Aeson auxiliary
-------------------------------------------------------------------------------}

-- | Combinator for data type that want to keep the full unparsed value
keepValue :: (b -> Value -> a)
          -> (Value -> Parser b)
          -> (Value -> Parser a)
keepValue f p v = flip f v <$> p v

stringOr :: (Either String b -> a)
         -> (Value -> Parser b)
         -> (Value -> Parser a)
stringOr f _ (String str) = return (f (Left (T.unpack str)))
stringOr f p v            = f . Right <$> p v

withString :: String
           -> (String -> Parser a)
           -> (Value  -> Parser a)
withString _    p (String str) = p (T.unpack str)
withString err  _ wat          = typeMismatch err wat

{-------------------------------------------------------------------------------
  Read the file
-------------------------------------------------------------------------------}

readStackYaml :: FilePath -> IO StackYaml
readStackYaml fp = decodeFileEither fp >>= either throwIO return
