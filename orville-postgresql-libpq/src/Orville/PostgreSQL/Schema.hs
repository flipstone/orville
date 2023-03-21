{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Orville.PostgreSQL.Schema
  ( module Orville.PostgreSQL.Schema.TableDefinition,
    module Orville.PostgreSQL.Schema.TableIdentifier,
    module Orville.PostgreSQL.Schema.PrimaryKey,
    module Orville.PostgreSQL.Schema.IndexDefinition,
    module Orville.PostgreSQL.Schema.ConstraintDefinition,
    module Orville.PostgreSQL.Schema.SequenceDefinition,
    module Orville.PostgreSQL.Schema.SequenceIdentifier,
  )
where

-- Note: we list the re-exports explicity above to control the order that they
-- appear in the generated haddock documentation.

import Orville.PostgreSQL.Schema.ConstraintDefinition
import Orville.PostgreSQL.Schema.IndexDefinition
import Orville.PostgreSQL.Schema.PrimaryKey
import Orville.PostgreSQL.Schema.SequenceDefinition
import Orville.PostgreSQL.Schema.SequenceIdentifier
import Orville.PostgreSQL.Schema.TableDefinition
import Orville.PostgreSQL.Schema.TableIdentifier
