{-# LANGUAGE GADTs #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Functions for working with executable @UPDATE@ statements. The 'Update' type is
a value that can be passed around and executed later. The 'Update' is directly
associated with the presence of a returning clause and how to decode any rows
returned by that clause. This means it can be safely executed via
'executeUpdate' or 'executeUpdateReturnEntities' as appropriate. It is a
lower-level API than the entity update functions in
"Orville.PostgreSQL.Execution.EntityOperations", but not as primitive as
"Orville.PostgreSQL.Expr.Update".

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.Update
  ( Update
  , updateToUpdateExpr
  , executeUpdate
  , executeUpdateReturnEntities
  , updateToTableReturning
  , updateToTable
  , updateToTableFieldsReturning
  , updateToTableFields
  , rawUpdateExpr
  )
where

import Data.List.NonEmpty (NonEmpty, nonEmpty)

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import Orville.PostgreSQL.Execution.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall (AnnotatedSqlMarshaller, marshallEntityToSetClauses, unannotatedSqlMarshaller)
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (HasKey, TableDefinition, mkTableReturningClause, primaryKeyEquals, tableMarshaller, tableName, tablePrimaryKey)

{- |
  Represents an @UPDATE@ statement that can be executed against a database. An
  'Update' has a 'Orville.PostgreSQL.SqlMarshaller' bound to it that, when the
  update returns data from the database, will be used to decode the database
  result set when it is executed.

@since 1.0.0.0
-}
data Update readEntity returningClause where
  UpdateNoReturning :: Expr.UpdateExpr -> Update readEntity NoReturningClause
  UpdateReturning :: AnnotatedSqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity ReturningClause

{- |
  Extracts the query that will be run when the update is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.

@since 1.0.0.0
-}
updateToUpdateExpr :: Update readEntity returningClause -> Expr.UpdateExpr
updateToUpdateExpr (UpdateNoReturning expr) = expr
updateToUpdateExpr (UpdateReturning _ expr) = expr

{- |
  Executes the database query for the 'Update' and returns the number of
  affected rows.

@since 1.0.0.0
-}
executeUpdate :: Monad.MonadOrville m => Update readEntity returningClause -> m Int
executeUpdate =
  Execute.executeAndReturnAffectedRows QueryType.UpdateQuery . updateToUpdateExpr

{- |
  Executes the database query for the 'Update' and uses its
  'AnnotatedSqlMarshaller' to decode any rows that were just updated, as
  returned via a RETURNING clause.

@since 1.0.0.0
-}
executeUpdateReturnEntities :: Monad.MonadOrville m => Update readEntity ReturningClause -> m [readEntity]
executeUpdateReturnEntities (UpdateReturning marshaller expr) =
  Execute.executeAndDecode QueryType.UpdateQuery expr marshaller

{- |
  Builds an 'Update' that will update all of the writable columns described in
  the 'TableDefinition' without returning the data as seen by the database.

  This function returns 'Nothing' if the 'TableDefinition' has no columns,
  which would otherwise generate and 'Update' with invalid SQL syntax.

@since 1.0.0.0
-}
updateToTable ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Maybe (Update readEntity NoReturningClause)
updateToTable =
  updateTable WithoutReturning

{- |
  Builds an 'Update' that will update all of the writable columns described in
  the 'TableDefinition' and return the data as seen by the database. This is
  useful for getting database-managed columns such as auto-incrementing
  identifiers and sequences.

  This function returns 'Nothing' if the 'TableDefinition' has no columns,
  which would otherwise generate an 'Update' with invalid SQL syntax.

@since 1.0.0.0
-}
updateToTableReturning ::
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Maybe (Update readEntity ReturningClause)
updateToTableReturning =
  updateTable WithReturning

{- |
  Builds an 'Update' that will apply the specified column set clauses to rows
  within the specified table without returning the data as seen by the database.

@since 1.0.0.0
-}
updateToTableFields ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  Update readEntity NoReturningClause
updateToTableFields =
  updateFields WithoutReturning

{- |
  Builds an 'Update' that will apply the specified column set clauses to rows
  within the specified table and return the updated version of any rows affected by
  the update state by using a @RETURNING@ clause.

@since 1.0.0.0
-}
updateToTableFieldsReturning ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  Update readEntity ReturningClause
updateToTableFieldsReturning =
  updateFields WithReturning

{- |
  Builds an 'Update' that will execute the specified query and use the given 'AnnotatedSqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.UpdateExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'AnnotatedSqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Update'. The caller can build any query
  that Orville supports using the expression-building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.UpdateExpr'.

@since 1.0.0.0
-}
rawUpdateExpr :: ReturningOption returningClause -> AnnotatedSqlMarshaller writeEntity readEntity -> Expr.UpdateExpr -> Update readEntity returningClause
rawUpdateExpr WithReturning marshaller = UpdateReturning marshaller
rawUpdateExpr WithoutReturning _ = UpdateNoReturning

-- an internal helper function for creating an update with a given
-- `ReturningOption` to a single entity in a table, setting all the
-- columns found in the table's SQL marshaller.
updateTable ::
  ReturningOption returningClause ->
  TableDefinition (HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  Maybe (Update readEntity returningClause)
updateTable returningOption tableDef key writeEntity = do
  setClauses <-
    nonEmpty $
      marshallEntityToSetClauses
        (unannotatedSqlMarshaller $ tableMarshaller tableDef)
        writeEntity

  let
    isEntityKey =
      primaryKeyEquals
        (tablePrimaryKey tableDef)
        key
  pure $
    updateFields
      returningOption
      tableDef
      setClauses
      (Just isEntityKey)

-- an internal helper function for creating an update with a given
-- `ReturningOption` to update the specified columns.
updateFields ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  Update readEntity returningClause
updateFields returingOption tableDef setClauses mbWhereCondition =
  let
    whereClause =
      fmap Expr.whereClause mbWhereCondition
  in
    rawUpdateExpr returingOption (tableMarshaller tableDef) $
      Expr.updateExpr
        (tableName tableDef)
        (Expr.setClauseList setClauses)
        whereClause
        (mkTableReturningClause returingOption tableDef)
