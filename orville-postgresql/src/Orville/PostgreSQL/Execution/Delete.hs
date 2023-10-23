{-# LANGUAGE GADTs #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Functions for working with executable @DELETE@ statements. The 'Delete' type is
a value that can be passed around and executed later. The 'Delete' is directly
associated with the presence of a returning clause and how to decode any rows
returned by that clause. This means it can be safely executed via
'executeDelete' or 'executeDeleteReturnEntities' as appropriate. It is a lower-level
API than the entity delete functions in
"Orville.PostgreSQL.Execution.EntityOperations", but not as primitive as
"Orville.PostgreSQL.Expr.Delete".

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.Delete
  ( Delete
  , deleteFromDeleteExpr
  , executeDelete
  , executeDeleteReturnEntities
  , deleteFromTableReturning
  , deleteFromTable
  , rawDeleteExpr
  )
where

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import Orville.PostgreSQL.Execution.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller)
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (TableDefinition, mkTableReturningClause, tableMarshaller, tableName)

{- |
Represents a @DELETE@ statement that can be executed against a database. A
'Delete' has a 'Orville.PostgreSQL.SqlMarshaller' bound to it that, when the
delete returns data from the database, will be used to decode the database
result set when it is executed.

@since 1.0.0.0
-}
data Delete readEntity returningClause where
  Delete ::
    Expr.DeleteExpr ->
    Delete readEntity NoReturningClause
  DeleteReturning :: AnnotatedSqlMarshaller writeEntity readEntity -> Expr.DeleteExpr -> Delete readEntity ReturningClause

{- |
  Extracts the query that will be run when the delete is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.

@since 1.0.0.0
-}
deleteFromDeleteExpr :: Delete readEntity returningClause -> Expr.DeleteExpr
deleteFromDeleteExpr (Delete expr) = expr
deleteFromDeleteExpr (DeleteReturning _ expr) = expr

{- |
  Executes the database query for the 'Delete' and returns the number of
  rows affected by the query.

@since 1.0.0.0
-}
executeDelete ::
  Monad.MonadOrville m =>
  Delete readEntity NoReturningClause ->
  m Int
executeDelete (Delete expr) =
  Execute.executeAndReturnAffectedRows QueryType.DeleteQuery expr

{- |
Executes the database query for the 'Delete' and uses its
'Orville.PostgreSQL.SqlMarshaller' to decode the rows (that were just deleted)
as returned via a RETURNING clause.

@since 1.0.0.0
-}
executeDeleteReturnEntities ::
  Monad.MonadOrville m =>
  Delete readEntity ReturningClause ->
  m [readEntity]
executeDeleteReturnEntities (DeleteReturning marshaller expr) =
  Execute.executeAndDecode QueryType.DeleteQuery expr marshaller

{- |
  Builds a 'Delete' that will delete all of the writable columns described in the
  'TableDefinition' without returning the data as seen by the database.

@since 1.0.0.0
-}
deleteFromTable ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  Delete readEntity NoReturningClause
deleteFromTable =
  deleteTable WithoutReturning

{- |
  Builds a 'Delete' that will delete all of the writable columns described in the
  'TableDefinition' and return the data as seen by the database. This is useful for getting
  database-managed columns such as auto-incrementing identifiers and sequences.

@since 1.0.0.0
-}
deleteFromTableReturning ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  Delete readEntity ReturningClause
deleteFromTableReturning =
  deleteTable WithReturning

-- an internal helper function for creating a delete with a given `ReturningOption`
deleteTable ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  Delete readEntity returningClause
deleteTable returningOption tableDef whereCondition =
  let
    deleteExpr =
      Expr.deleteExpr
        (tableName tableDef)
        (fmap Expr.whereClause whereCondition)
        (mkTableReturningClause returningOption tableDef)
  in
    rawDeleteExpr returningOption (tableMarshaller tableDef) deleteExpr

{- |
  Builds a 'Delete' that will execute the specified query and use the given
  'Orville.PostgreSQL.SqlMarshaller' to decode it. It is up to the caller to
  ensure that the given 'Expr.DeleteExpr' makes sense and returns a result that
  the 'Orville.PostgreSQL.SqlMarshaller' can decode.

  This is the lowest level of escape hatch available for 'Delete'. The caller can build any query
  that Orville supports using the expression-building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.DeleteExpr'. It is expected that the 'ReturningOption' given matches the
  'Expr.DeleteExpr'. This level of interface does not provide an automatic enforcement of the
  expectation, however failure is likely if that is not met.

@since 1.0.0.0
-}
rawDeleteExpr :: ReturningOption returningClause -> AnnotatedSqlMarshaller writeEntity readEntity -> Expr.DeleteExpr -> Delete readEntity returningClause
rawDeleteExpr WithReturning marshaller = DeleteReturning marshaller
rawDeleteExpr WithoutReturning _ = Delete
