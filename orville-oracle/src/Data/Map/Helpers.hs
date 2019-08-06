{-|
Module    : Data.Map.Helpers
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Data.Map.Helpers
  ( groupBy
  , groupBy'
  ) where

import qualified Data.Map.Strict as Map

groupBy :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
groupBy keyFunc = groupBy' mkEntry
  where
    mkEntry a = (keyFunc a, a)

groupBy' :: Ord k => (a -> (k, v)) -> [a] -> Map.Map k [v]
groupBy' mkEntry as = Map.fromListWith (++) (map mkListEntry as)
  where
    mkListEntry a =
      let (k, v) = mkEntry a
       in (k, [v])
