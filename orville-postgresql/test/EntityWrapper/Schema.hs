module EntityWrapper.Schema
  ( module EntityWrapper.Schema
  , module EntityWrapper.Schema.Virus
  ) where

import qualified Database.Orville.PostgreSQL as O

import EntityWrapper.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
