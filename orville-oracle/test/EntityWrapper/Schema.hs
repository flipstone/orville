module EntityWrapper.Schema
  ( module EntityWrapper.Schema
  , module EntityWrapper.Schema.Virus
  ) where

import qualified Database.Orville.Oracle as O

import EntityWrapper.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
