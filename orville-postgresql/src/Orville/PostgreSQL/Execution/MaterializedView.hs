{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

Operations on materialized views that are neither selects nor schema
migration, in the way that "Orville.PostgreSQL.Execution.Sequence" holds the
operations on sequences.

@since 1.2.0.0
-}
module Orville.PostgreSQL.Execution.MaterializedView
  ( refreshMaterializedView
  , MaterializedViewRefresh (RefreshConcurrently, RefreshExclusively, RefreshWithNoData)
  )
where

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (MaterializedViewDefinition, materializedViewDefinitionName)

{- | How @REFRESH MATERIALIZED VIEW@ replaces the stored rows of a
  materialized view. PostgreSQL does not allow @CONCURRENTLY@ together with
  @WITH NO DATA@, so that combination cannot be expressed.

@since 1.2.0.0
-}
data MaterializedViewRefresh
  = {- | Refresh with @CONCURRENTLY@, so that the materialized view stays
    readable while its rows are replaced. PostgreSQL requires a unique index
    on the materialized view that covers all of its rows for this.
    -}
    RefreshConcurrently
  | {- | Replace the stored rows with the current result of the query
    (@WITH DATA@) while holding an exclusive lock on the materialized view,
    which blocks reads until the refresh finishes.
    -}
    RefreshExclusively
  | {- | Discard the stored rows and leave the materialized view unreadable
    until it is refreshed again (@WITH NO DATA@).
    -}
    RefreshWithNoData
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Show
    )

{- | Runs @REFRESH MATERIALIZED VIEW@ for the given definition.

@since 1.2.0.0
-}
refreshMaterializedView ::
  Monad.MonadOrville m =>
  MaterializedViewDefinition readEntity ->
  MaterializedViewRefresh ->
  m ()
refreshMaterializedView viewDef refresh =
  let
    (mbConcurrently, withDataExpr) =
      case refresh of
        RefreshConcurrently -> (Just Expr.concurrently, Expr.withData)
        RefreshExclusively -> (Nothing, Expr.withData)
        RefreshWithNoData -> (Nothing, Expr.withNoData)
  in
    Execute.executeVoid QueryType.OtherQuery $
      Expr.refreshMaterializedViewExpr
        mbConcurrently
        (materializedViewDefinitionName viewDef)
        (Just withDataExpr)
