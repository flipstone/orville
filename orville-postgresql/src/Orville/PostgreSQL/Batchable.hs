{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Batchable
  ( Batchable
  , BatchSize (..)
  , batchNonEmpty
  , mapBatchable
  , toBatched
  , toUnbatched
  ) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE

{- | 'BatchSize' controls how many items are included in each batch when a
  'Batchable' is resolved via 'toBatched'.

  * 'BatchSizeAuto' uses the automatic batch size that was determined when
    the 'Batchable' was created (e.g. based on PostgreSQL's parameter limit).
  * @'BatchSize' n@ uses an explicit batch size of @n@ items per batch.

@since 1.2.0.0
-}
data BatchSize
  = BatchSizeAuto
  | BatchSize Int
  deriving (Show)

{- | A 'Batchable' represents a value that can either be used as a single
  unit or split into multiple batches. This is used to support executing
  large insert operations in batches that stay within PostgreSQL's parameter
  limit, while also allowing the same operation to be executed as a single
  statement when batching is not needed.

  This type is designed to allow batching to be provided as an option while
  incurring no actual batching overhead when it's not actually desired.

@since 1.2.0.0
-}
newtype Batchable a
  = Batchable (forall f. Batching f -> f a)

instance Functor Batchable where
  fmap = mapBatchable

data Batching (f :: Type -> Type) where
  Unbatched :: Batching Identity
  Batched :: BatchSize -> Batching []

{- | Maps a function over both the batched and unbatched representations of a
  'Batchable'.

@since 1.2.0.0
-}
{-# INLINEABLE mapBatchable #-}
mapBatchable :: (a -> b) -> Batchable a -> Batchable b
mapBatchable f (Batchable g) =
  Batchable (\batching -> mapBatching batching f (g batching))

-- This uses multiple function branches rather than a
-- case statement to help GHC fully inline and specialize
-- this in the hopes that 'Identity' will be completely
-- erased at runtime
mapBatching :: Batching f -> (a -> b) -> f a -> f b
mapBatching Unbatched = fmap
mapBatching (Batched _) = fmap

{- | Resolves a 'Batchable' as a single unbatched value. No batching logic is
  applied; the original value is returned directly.

@since 1.2.0.0
-}
{-# INLINEABLE toUnbatched #-}
toUnbatched :: Batchable a -> a
toUnbatched (Batchable f) =
  runIdentity (f Unbatched)

{- | Resolves a 'Batchable' into a list of batched values according to the
  given 'BatchSize'.

@since 1.2.0.0
-}
{-# INLINEABLE toBatched #-}
toBatched :: BatchSize -> Batchable a -> [a]
toBatched batchSize (Batchable f) =
  f (Batched batchSize)

{- | Creates a 'Batchable' from a 'NE.NonEmpty' list. When resolved via
  'toUnbatched', the original list is returned unchanged. When resolved via
  'toBatched', the list is split into chunks of the appropriate size.

  Note: The @autoBatchSize@ parameter is lazy — it will only be evaluated if
  the 'Batchable' is later resolved via 'toBatched' with 'BatchSizeAuto'. This
  means callers can provide a potentially expensive batch size calculation
  without paying the cost of the calculation when the 'Batchable' is resolved
  via 'toUnbatched'.

@since 1.2.0.0
-}
batchNonEmpty ::
  Int ->
  NE.NonEmpty a ->
  Batchable (NE.NonEmpty a)
batchNonEmpty autoBatchSize source =
  Batchable $ \batching ->
    case batching of
      Unbatched -> Identity source
      Batched batchSize ->
        let
          itemsPerBatch =
            case batchSize of
              BatchSizeAuto -> autoBatchSize
              BatchSize n -> n

          go rest =
            case rest of
              [] -> []
              _ ->
                let
                  (batch, remaining) =
                    List.splitAt itemsPerBatch rest
                in
                  maybe [] (: go remaining) (NE.nonEmpty batch)
        in
          go (NE.toList source)
