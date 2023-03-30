{-# LANGUAGE GADTs #-}

{- |

Module    : Orville.PostgreSQL.Internal.Delete
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Delete
  ( Delete,
    deleteFromDeleteExpr,
    executeDelete,
    executeDeleteReturnEntities,
    deleteFromTableReturning,
    deleteFromTable,
    rawDeleteExpr,
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.QueryType as QueryType
import Orville.PostgreSQL.Internal.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller)
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (TableDefinition, mkTableReturningClause, tableMarshaller, tableName)

{- | Represents a @DELETE@ statement that can be executed against a database. A 'Delete' has a
  'SqlMarshaller' bound to it that, when the delete returns data from the database, will be used to
  decode the database result set when it is executed.
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
-}
deleteFromDeleteExpr :: Delete readEntity returningClause -> Expr.DeleteExpr
deleteFromDeleteExpr (Delete expr) = expr
deleteFromDeleteExpr (DeleteReturning _ expr) = expr

{- |
  Excutes the database query for the 'Delete' and returns the number of
  rows affected by the query.
-}
executeDelete ::
  Monad.MonadOrville m =>
  Delete readEntity NoReturningClause ->
  m Int
executeDelete (Delete expr) =
  Execute.executeAndReturnAffectedRows QueryType.DeleteQuery expr

{- | Excutes the database query for the 'Delete' and uses its 'SqlMarshaller' to decode the rows (that
  were just deleteed) as returned via a RETURNING clause.
-}
executeDeleteReturnEntities ::
  Monad.MonadOrville m =>
  Delete readEntity ReturningClause ->
  m [readEntity]
executeDeleteReturnEntities (DeleteReturning marshaller expr) =
  Execute.executeAndDecode QueryType.DeleteQuery expr marshaller

{- |
  Builds a 'Delete' that will delete all of the writeable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
deleteFromTable ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  Delete readEntity NoReturningClause
deleteFromTable =
  deleteTable WithoutReturning

{- |
  Builds a 'Delete' that will delete all of the writeable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
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
  let deleteExpr =
        Expr.deleteExpr
          (tableName tableDef)
          (fmap Expr.whereClause whereCondition)
          (mkTableReturningClause returningOption tableDef)
   in rawDeleteExpr returningOption (tableMarshaller tableDef) deleteExpr

{- |
  Builds a 'Delete' that will execute the specified query and use the given
  'SqlMarshaller' to decode it. It is up to the caller to ensure that the given
  'Expr.DeleteExpr' makes sense and returns a result that the 'SqlMarshaller'
  can decode.

  This is the lowest level of escape hatch available for 'Delete'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.DeleteExpr'. It is expected that the 'ReturningOption' given matches the
  'Expr.DeleteExpr'. This level of interface does not provide an automatic enforcement of the
  expectation, however failure is likely if that is not met.
-}
rawDeleteExpr :: ReturningOption returningClause -> AnnotatedSqlMarshaller writeEntity readEntity -> Expr.DeleteExpr -> Delete readEntity returningClause
rawDeleteExpr WithReturning marshaller = DeleteReturning marshaller
rawDeleteExpr WithoutReturning _ = Delete
