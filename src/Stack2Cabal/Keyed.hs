module Stack2Cabal.Keyed (
    Keyed -- opaque
  , empty
  , fromList
  , toList
  , zip
  ) where

import Prelude hiding (zip)
import Control.Applicative hiding (empty)
import Data.Aeson.Types
import Data.Map.Strict (Map)
import Generics.SOP hiding (fromList)

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T

{-------------------------------------------------------------------------------
  Keyed data
-------------------------------------------------------------------------------}

data Keyed a = Keyed { toMap :: Map String a }
  deriving (Show)

empty :: Keyed a
empty = fromMap Map.empty

fromMap :: Map String a -> Keyed a
fromMap = Keyed

fromList :: [(String, a)] -> Keyed a
fromList = fromMap . Map.fromList

toList :: Keyed a-> [(String, a)]
toList = Map.toList . toMap

{-------------------------------------------------------------------------------
  Misc operations
-------------------------------------------------------------------------------}

zip :: forall as. SListI as => NP Keyed as -> Keyed (NP Maybe as)
zip = fromMap
    . Map.unionsWith (hliftA2 (<|>))
    . hcollapse
    . hliftA2 aux expansions
    . hliftA toMap
  where
    aux :: Expansion as a -> Map String a -> K (Map String (NP Maybe as)) a
    aux e = K . Map.map (unK . apFn e . I)

{-------------------------------------------------------------------------------
  Auxiliary generics-SOP
-------------------------------------------------------------------------------}

type Expansion (xs :: [*]) = I -.-> K (NP Maybe xs)

shiftExpansion :: Expansion xs a -> Expansion (x ': xs) a
shiftExpansion (Fn f) = Fn $ K . (Nothing :*) . unK . f

expansions :: forall xs. SListI xs => NP (Expansion xs) xs
expansions =
    case sList :: SList xs of
      SNil  -> Nil
      SCons -> fn (\(I x) -> K $ Just x :* hpure Nothing)
            :* hliftA shiftExpansion expansions

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
