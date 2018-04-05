module Example.Schema
  ( module Example.Schema
  , module Example.Schema.Virus
  ) where

import qualified Database.Orville as O

import Example.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
