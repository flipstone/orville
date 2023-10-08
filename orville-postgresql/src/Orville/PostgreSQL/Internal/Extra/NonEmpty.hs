{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.Extra.NonEmpty
  ( foldl1'
  )
where

import qualified Data.Foldable as Fold
import Data.List.NonEmpty (NonEmpty ((:|)))

foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (first :| rest) =
  Fold.foldl' f first rest
