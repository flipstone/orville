{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

Entity select operations that work with any 'Select.Selectable' definition,
such as a 'Orville.PostgreSQL.ReadOnlyTableDefinition' describing a join, in
addition to a 'Orville.PostgreSQL.TableDefinition'. The functions in
"Orville.PostgreSQL.Execution.EntityOperations" are type specialized versions
of these for 'Orville.PostgreSQL.TableDefinition'.

@since 1.2.0.0
-}
module Orville.PostgreSQL.Execution.SelectableEntityOperations
  ( findSelectableEntitiesBy
  , findFirstSelectableEntityBy
  )
where

import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Execution.Select as Select
import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Monad as Monad

{- | Finds all the entities in the given 'Select.Selectable' definition according to the specified
  'SelectOptions.SelectOptions'.

@since 1.2.0.0
-}
findSelectableEntitiesBy ::
  (Monad.MonadOrville m, Select.Selectable table) =>
  table readEntity ->
  SelectOptions.SelectOptions ->
  m [readEntity]
findSelectableEntitiesBy selectable =
  Select.executeSelect
    . Select.selectTable selectable

{- | Like 'findSelectableEntitiesBy', but adds a 'LIMIT 1' to the query and then returns the first
  item from the list. Usually when you use this you will want to provide an order by clause in the
  'SelectOptions.SelectOptions', or otherwise ensure a stable ordering because PostgreSQL does not
  guarantee order by default.

@since 1.2.0.0
-}
findFirstSelectableEntityBy ::
  (Monad.MonadOrville m, Select.Selectable table) =>
  table readEntity ->
  SelectOptions.SelectOptions ->
  m (Maybe readEntity)
findFirstSelectableEntityBy selectable selectOptions =
  fmap listToMaybe $
    findSelectableEntitiesBy selectable (SelectOptions.limit 1 <> selectOptions)
