{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.Update
  ( Update,
    updateToUpdateExpr,
    executeUpdate,
    executeUpdateReturnEntity,
    updateToTableReturning,
    updateToTable,
  )
where

import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (HasKey, TableDefinition, mkUpdateExpr, tableMarshaller)

{- | Represents an @UPDATE@ statement that can be executed against a database. An 'Update' has a
  'SqlMarshaller' bound to it that, when the update returns data from the database, will be used to
  decode the database result set when it is executed.
-}
data Update readEntity returningClause where
  UpdateNoReturning :: SqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity NoReturningClause
  UpdateReturning :: SqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity ReturningClause

{- |
  Extracts the query that will be run when the update is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
updateToUpdateExpr :: Update readEntity returningClause -> Expr.UpdateExpr
updateToUpdateExpr (UpdateNoReturning _ expr) = expr
updateToUpdateExpr (UpdateReturning _ expr) = expr

{- |
  Executes the database query for the 'Update' and returns '()'.
-}
executeUpdate :: MonadOrville.MonadOrville m => Update readEntity returningClause -> m ()
executeUpdate =
  Execute.executeVoid . updateToUpdateExpr

{- | Executes the database query for the 'Update' and uses its 'SqlMarshaller' to decode the row (that
  was just updated) as returned via a RETURNING clause.
-}
executeUpdateReturnEntity :: MonadOrville.MonadOrville m => Update readEntity ReturningClause -> m (Maybe readEntity)
executeUpdateReturnEntity (UpdateReturning marshaller expr) =
  fmap listToMaybe $ Execute.executeAndDecode expr marshaller

{- |
  Builds an 'Update' that will update all of the writable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
updateToTable ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity NoReturningClause
updateToTable =
  updateTable WithoutReturning

{- |
  Builds an 'Update' that will update all of the writable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
-}
updateToTableReturning ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity ReturningClause
updateToTableReturning =
  updateTable WithReturning

-- an internal helper function for creating an update with a given `ReturningOption`
updateTable ::
  ReturningOption returningClause ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity returningClause
updateTable returningOption tableDef key writeEntity =
  rawUpdateExpr returningOption (tableMarshaller tableDef) (mkUpdateExpr returningOption tableDef key writeEntity)

{- |
  Builds an 'Update' that will execute the specified query and use the given 'SqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.UpdateExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'SqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Select'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.UpdateExpr'.
-}
rawUpdateExpr :: ReturningOption returningClause -> SqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity returningClause
rawUpdateExpr WithReturning = UpdateReturning
rawUpdateExpr WithoutReturning = UpdateNoReturning
