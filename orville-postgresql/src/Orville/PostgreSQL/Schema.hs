{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

You can import "Orville.PostgreSQL.Schema" to get access to all the functions
related to representing a SQL schema. This includes a number of lower-level
items not exported by "Orville.PostgreSQL" that give you more control (and
therefore responsibility) over the definition of the schema.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema
  ( -- * Defining Tables
    module Orville.PostgreSQL.Schema.TableDefinition
  , module Orville.PostgreSQL.Schema.TableIdentifier
  , module Orville.PostgreSQL.Schema.PrimaryKey
  , module Orville.PostgreSQL.Schema.IndexDefinition
  , module Orville.PostgreSQL.Schema.ConstraintDefinition
  , module Orville.PostgreSQL.Schema.TriggerDefinition

    -- * Defining Sequences
  , module Orville.PostgreSQL.Schema.SequenceDefinition
  , module Orville.PostgreSQL.Schema.SequenceIdentifier

    -- * Definining Functions
  , module Orville.PostgreSQL.Schema.FunctionDefinition
  , module Orville.PostgreSQL.Schema.FunctionIdentifier

    -- * Using PostgreSQL Extensions
  , module Orville.PostgreSQL.Schema.ExtensionIdentifier
  )
where

-- Note: we list the re-exports explicity above to control the order that they
-- appear in the generated haddock documentation.

import Orville.PostgreSQL.Schema.ConstraintDefinition
import Orville.PostgreSQL.Schema.ExtensionIdentifier
import Orville.PostgreSQL.Schema.FunctionDefinition
import Orville.PostgreSQL.Schema.FunctionIdentifier
import Orville.PostgreSQL.Schema.IndexDefinition
import Orville.PostgreSQL.Schema.PrimaryKey
import Orville.PostgreSQL.Schema.SequenceDefinition
import Orville.PostgreSQL.Schema.SequenceIdentifier
import Orville.PostgreSQL.Schema.TableDefinition
import Orville.PostgreSQL.Schema.TableIdentifier
import Orville.PostgreSQL.Schema.TriggerDefinition
