module ParameterizedEntity.Schema
  ( module ParameterizedEntity.Schema
  , module ParameterizedEntity.Schema.Virus
  ) where

import qualified Database.Orville.Oracle as O

import ParameterizedEntity.Schema.Virus

schema :: O.SchemaDefinition
schema = [O.Table virusTable]
