{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.Extra.NonEmpty
  ( foldl1'
  , foldMap1'
  )
where

import qualified Data.Foldable as Fold
import Data.List.NonEmpty (NonEmpty ((:|)))

{- | A variant of foldl' that safely does not need a base case.

@since 1.0.0.0
-}
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (first :| rest) =
  Fold.foldl' f first rest

{- | A variant of foldMap' that safely does not need a base case.

@since 1.0.0.0
-}
foldMap1' :: Semigroup m => (a -> m) -> NonEmpty a -> m
foldMap1' f (first :| rest) =
  Fold.foldl' (\m a -> m <> f a) (f first) rest
