{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

You can import "Orville.PostgreSQL.Execution" to get access to all the
execution-related functions and types exported by the modules below. This
includes a number of lowel-level items not exported by "Orville.PostgreSQL"
that give you more control (and therefore responsibility) over how the SQL is
executed.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution
  ( -- * High level modules for most common tasks
      module Orville.PostgreSQL.Execution.EntityOperations
  , module Orville.PostgreSQL.Execution.SelectOptions
  , module Orville.PostgreSQL.Execution.Sequence
  , module Orville.PostgreSQL.Execution.Transaction

    -- * Mid-level modules with more flexibility for handling less common queries
  , module Orville.PostgreSQL.Execution.Select
  , module Orville.PostgreSQL.Execution.Insert
  , module Orville.PostgreSQL.Execution.Update
  , module Orville.PostgreSQL.Execution.Delete
  , module Orville.PostgreSQL.Execution.Cursor
  , module Orville.PostgreSQL.Execution.ReturningOption

    -- * Low-level modules for executing and decoding SQL expressions yourself
  , module Orville.PostgreSQL.Execution.Execute
  , module Orville.PostgreSQL.Execution.ExecutionResult
  , module Orville.PostgreSQL.Execution.QueryType
  )
where

-- Note: we list the re-exports explicity above to control the order that they
-- appear in the generated haddock documentation.

import Orville.PostgreSQL.Execution.Cursor
import Orville.PostgreSQL.Execution.Delete
import Orville.PostgreSQL.Execution.EntityOperations
import Orville.PostgreSQL.Execution.Execute
import Orville.PostgreSQL.Execution.ExecutionResult
import Orville.PostgreSQL.Execution.Insert
import Orville.PostgreSQL.Execution.QueryType
import Orville.PostgreSQL.Execution.ReturningOption
import Orville.PostgreSQL.Execution.Select
import Orville.PostgreSQL.Execution.SelectOptions
import Orville.PostgreSQL.Execution.Sequence
import Orville.PostgreSQL.Execution.Transaction
import Orville.PostgreSQL.Execution.Update
