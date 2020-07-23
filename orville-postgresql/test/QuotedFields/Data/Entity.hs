module QuotedFields.Data.Entity
( NonNullableEntity(..)
, NullableEntity(..)
, DroppedEntity(..)
) where

import Data.Int(Int32)

data NonNullableEntity = NonNullableEntity
  { nonNullSnakeCase :: Int32
  , nonNullCamelCase :: Int32
  , nonNullOrder :: Int32
  } deriving (Show, Eq)

data NullableEntity = NullableEntity
  { nullableSnakeCase :: Int32
  , nullableCamelCase :: Maybe Int32
  , nullableOrder :: Maybe Int32
  } deriving (Show, Eq)

data DroppedEntity = DroppedEntity
  { droppedSnakeCase :: Int32
  } deriving (Show, Eq)
