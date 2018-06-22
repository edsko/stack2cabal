module Stack2Cabal.Util (
    splitWhen
  ) where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs =
    case break p xs of
      (prefix, [])               -> [prefix]
      (prefix, _match:remainder) -> prefix : splitWhen p remainder
