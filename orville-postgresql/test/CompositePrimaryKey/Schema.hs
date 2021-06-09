module CompositePrimaryKey.Schema
  ( module CompositePrimaryKey.Schema
  , module CompositePrimaryKey.Schema.Virus
  ) where

import qualified Database.Orville.PostgreSQL as O

import CompositePrimaryKey.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
