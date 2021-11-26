{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.Update
  ( Update,
    updateToUpdateExpr,
    executeUpdate,
    executeUpdateReturnEntities,
    updateToTableReturning,
    updateToTable,
  )
where

import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (HasKey, ReturningOption (WithReturning, WithoutReturning), TableDefinition, mkUpdateExpr, tableMarshaller)

{- | Represents an @UPDATE@ statement that can be executed against a database. An 'Update' has a
  'SqlMarshaller' bound to it that, when the update returns data from the database, will be used to
  decode the database result set when it is executed.
-}
data Update readEntity where
  Update :: SqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity

{- |
  Extracts the query that will be run when the update is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
updateToUpdateExpr :: Update readEntity -> Expr.UpdateExpr
updateToUpdateExpr (Update _ expr) = expr

{- |
  Excutes the database query for the 'Update' and returns '()'.
-}
executeUpdate :: MonadOrville.MonadOrville m => Update readEntity -> m ()
executeUpdate (Update _ expr) =
  Execute.executeVoid expr

{- | Excutes the database query for the 'Update' and uses its 'SqlMarshaller' to decode the rows (that
  were just updateed) as returned via a RETURNING clause.
-}
executeUpdateReturnEntities :: MonadOrville.MonadOrville m => Update readEntity -> m (Maybe readEntity)
executeUpdateReturnEntities (Update marshaller expr) =
  fmap listToMaybe $ Execute.executeAndDecode expr marshaller

{- |
  Builds an 'Update' that will update all of the writeable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
updateToTable ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity
updateToTable =
  updateTable WithoutReturning

{- |
  Builds an 'Update' that will update all of the writeable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
-}
updateToTableReturning ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity
updateToTableReturning =
  updateTable WithReturning

-- an internal helper function for creating an update with a given `ReturningOption`
updateTable ::
  ReturningOption ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Update readEntity
updateTable returningOption tableDef key writeEntity =
  rawUpdateExpr (tableMarshaller tableDef) (mkUpdateExpr returningOption tableDef key writeEntity)

{- |
  Builds an 'Update' that will execute the specified query and use the given 'SqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.UpdateExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'SqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Select'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.UpdateExpr'.
-}
rawUpdateExpr :: SqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity
rawUpdateExpr = Update
