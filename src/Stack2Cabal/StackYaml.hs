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
import Control.Monad
import Data.Coerce (coerce)
import Data.Either
import Data.Foldable (asum)
import Data.List
import Data.YAML
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text (Text)

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
    , stackYamlRaw    :: Node
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

-- FromYAML newtype-helper for 'String'
newtype S = S String

instance FromYAML S where
  parseYAML = withStr "String" (pure . S . T.unpack)

instance FromYAML StackYaml where
  parseYAML = keepNode StackYaml $
      withMap "Stack yaml file" $ \obj -> do
        stackResolver   <- T.unpack <$> obj .:  "resolver"
        packages        <- obj .:  "packages"
        dependencies    <- obj .:? "extra-deps"  .!= []
        stackFlags      <- obj .:? "flags"       .!= Keyed.empty
        stackGhcOptions <- Keyed.map T.unpack <$> obj .:? "ghc-options" .!= Keyed.empty
        let (stackLocalPackages, remotePkgs) = partitionPackages     packages
            (stackExtraDeps,     remoteDeps) = partitionDependencies dependencies
            stackRemotePackages              = remotePkgs ++ remoteDeps
        return ParsedYaml{..}

instance FromYAML RemotePackage where
  parseYAML = withMap "remote package" $ \obj -> do
      LocationWithSubdirs remPkgLocation locSubdirs <- obj .: "location"
      remPkgSubdirs  <- ((locSubdirs <|>) . fmap (map T.unpack)) <$> obj .:? "subdirs"
      remPkgExtraDep <- obj .:? "extra-dep" .!= True
      return RemotePackage{..}

instance FromYAML Package where
  parseYAML = stringOr (either (Package . Left) (Package . Right)) $ parseYAML

instance FromYAML DependencyOrRemote where
  parseYAML val = asum [
      (DependencyOrRemote . Left)  <$> parseYAML val
    , (DependencyOrRemote . Right) <$> parseYAML val
    ]

instance FromYAML LocationWithSubdirs where
  parseYAML = withMap "location" $ \obj -> msum [
        do gitRepo   <- T.unpack <$> obj .:  "git"
           gitCommit <- T.unpack <$> obj .:  "commit"
           subdirs   <- fmap (map T.unpack) <$> obj .:? "subdirs"
           return (LocationWithSubdirs Git{..} subdirs)
      ]

instance FromYAML Dependency where
  parseYAML = withStr "extra-deps" $ parseDep
    where
      parseDep :: Text -> Parser Dependency
      parseDep str =
          case reverse (splitWhen (== '-') (T.unpack str)) of
            [] -> fail $ "Could not parse " ++ show str
            version:pkgRev -> do
              let pkg = intercalate "-" (reverse pkgRev)
              return $ Dependency pkg version

{-------------------------------------------------------------------------------
  Aeson auxiliary
-------------------------------------------------------------------------------}

-- | Combinator for data type that want to keep the full unparsed value
keepNode :: (b -> Node -> a)
          -> (Node -> Parser b)
          -> (Node -> Parser a)
keepNode f p v = flip f v <$> p v

stringOr :: (Either String b -> a)
         -> (Node -> Parser b)
         -> (Node -> Parser a)
stringOr f _ (Scalar (SStr str)) = return (f (Left $ T.unpack str))
stringOr f p v                   = f . Right <$> p v


{-------------------------------------------------------------------------------
  Read the file
-------------------------------------------------------------------------------}

readStackYaml :: FilePath -> IO StackYaml
readStackYaml fp = either fail return . decodeSingleDoc =<< B.readFile fp
  where
    decodeSingleDoc bs = case decodeStrict bs of
                           Right [x] -> Right x
                           Right _   -> Left "stack.yaml must contain only a single YAML document"
                           Left e    -> Left e
