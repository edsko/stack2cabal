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
import Data.Aeson.Types
import Data.Coerce
import Data.Map.Strict (Map)
import Generics.SOP hiding (fromList)
import Generics.SOP.NS
import Generics.SOP.NP hiding (fromList)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T

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

instance FromJSON a => FromJSON (Keyed a) where
  parseJSON = fmap fromList . parseMap parseJSON

{-------------------------------------------------------------------------------
  Auxiliary Aeson
-------------------------------------------------------------------------------}

parseMap :: forall b. (Value -> Parser b) -> (Value -> Parser [(String, b)])
parseMap p = withObject "map" $ \obj -> go (HM.toList obj)
  where
    go :: [(T.Text, Value)] -> Parser [(String, b)]
    go = mapM $ \(key, val) -> (T.unpack key, ) <$> p val
