module Stack2Cabal.Keyed (
    Keyed -- opaque
  , empty
  , fromMap
  , toMap
  , fromList
  , toList
  , map
  , unionsWith
  , zip
  ) where

import Prelude hiding (zip, map)
import Control.Applicative hiding (empty)
import Data.Coerce
import Data.Map.Strict (Map)
import Generics.SOP hiding (fromList)
import Generics.SOP.NS
import Generics.SOP.NP hiding (fromList)

import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import Data.YAML

{-------------------------------------------------------------------------------
  Keyed data
-------------------------------------------------------------------------------}

newtype Keyed a = Keyed { toMap :: Map String a }
  deriving (Show)

empty :: Keyed a
empty = fromMap Map.empty

fromMap :: Map String a -> Keyed a
fromMap = coerce

map :: (a -> b) -> Keyed a -> Keyed b
map = coerce . Map.map

unionsWith :: (a -> a -> a) -> [Keyed a] -> Keyed a
unionsWith = coerce . Map.unionsWith

fromList :: [(String, a)] -> Keyed a
fromList = fromMap . Map.fromList

toList :: Keyed a-> [(String, a)]
toList = Map.toList . toMap

{-------------------------------------------------------------------------------
  Misc operations
-------------------------------------------------------------------------------}

zip :: forall as. SListI as => NP Keyed as -> Keyed (NP Maybe as)
zip = unionsWith (zipWith_NP (<|>))
    . hcollapse
    . zipWith_NP aux injections
  where
    aux :: Injection I as a -> Keyed a -> K (Keyed (NP Maybe as)) a
    aux i = K . map (expandToMaybes . unK . apFn i . I)

{-------------------------------------------------------------------------------
  Auxiliary generics-SOP
-------------------------------------------------------------------------------}

expandToMaybes :: SListI as => NS I as -> NP Maybe as
expandToMaybes = expand_NS Nothing . map_NS (Just . unI)

{-------------------------------------------------------------------------------
  Aason support
-------------------------------------------------------------------------------}

instance FromYAML a => FromYAML (Keyed a) where
  parseYAML = fmap fromList . parseMap parseYAML

{-------------------------------------------------------------------------------
  Auxiliary Aeson
-------------------------------------------------------------------------------}

parseMap :: forall b. (Node -> Parser b) -> (Node -> Parser [(String, b)])
parseMap p = withMap "map" $ \obj -> go (Map.toList obj)
  where
    go :: [(Node, Node)] -> Parser [(String, b)]
    go = mapM $ \(key, val) -> (key2str key, ) <$> p val

    key2str :: Node -> String
    key2str (Scalar (SStr t)) = T.unpack t
    key2str _ = error "Keyed.parseMap: non-string key encountered in stack.yaml"
