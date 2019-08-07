module AppManagedEntity.Schema
  ( module AppManagedEntity.Schema
  , module AppManagedEntity.Schema.Virus
  ) where

import qualified Database.Orville.Oracle as O

import AppManagedEntity.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
