{-# LANGUAGE GADTs #-}

{- |

Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Functions for working with executable @INSERT@ statements. The 'Insert' type is
a value that can be passed around and executed later. The 'Insert' is directly
associated with the presence of a returning clause and how to decode any rows
returned by that clause. This means it can be safely executed via
'executeInsert' or 'executeInsertReturnEntities' as appropriate. It is a
lower-level API than the entity insert functions in
"Orville.PostgreSQL.Execution.EntityOperations", but not as primitive as
"Orville.PostgreSQL.Expr.Insert".

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.Insert
  ( Insert
  , insertToInsertExpr
  , executeInsert
  , executeInsertReturnEntities
  , insertToTableReturning
  , insertToTable
  , rawInsertExpr
  )
where

import Data.List.NonEmpty (NonEmpty)

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import Orville.PostgreSQL.Execution.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller)
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (TableDefinition, mkInsertExpr, tableMarshaller)

{- | Represents an @INSERT@ statement that can be executed against a database. An 'Insert' has a
  'SqlMarshaller' bound to it that, when the insert returns data from the database, will be used to
  decode the database result set when it is executed.

@since 1.0.0.0
-}
data Insert readEntity returningClause where
  Insert ::
    AnnotatedSqlMarshaller writeEntity readEntity ->
    Expr.InsertExpr ->
    Insert readEntity NoReturningClause
  InsertReturning :: AnnotatedSqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity ReturningClause

{- |
  Extracts the query that will be run when the insert is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.

@since 1.0.0.0
-}
insertToInsertExpr :: Insert readEntity returningClause -> Expr.InsertExpr
insertToInsertExpr (Insert _ expr) = expr
insertToInsertExpr (InsertReturning _ expr) = expr

{- |
  Excutes the database query for the 'Insert' and returns the number of rows
  affected by the query.

@since 1.0.0.0
-}
executeInsert ::
  Monad.MonadOrville m =>
  Insert readEntity NoReturningClause ->
  m Int
executeInsert (Insert _ expr) =
  Execute.executeAndReturnAffectedRows QueryType.InsertQuery expr

{- | Excutes the database query for the 'Insert' and uses its 'SqlMarshaller' to decode the rows (that
  were just inserted) as returned via a RETURNING clause.

@since 1.0.0.0
-}
executeInsertReturnEntities ::
  Monad.MonadOrville m =>
  Insert readEntity ReturningClause ->
  m [readEntity]
executeInsertReturnEntities (InsertReturning marshaller expr) =
  Execute.executeAndDecode QueryType.InsertQuery expr marshaller

{- |
  Builds an 'Insert' that will insert all of the writeable columns described in the
  'TableDefinition' without returning the data as seen by the database.

@since 1.0.0.0
-}
insertToTable ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity NoReturningClause
insertToTable =
  insertTable WithoutReturning

{- |
  Builds an 'Insert' that will insert all of the writeable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.

@since 1.0.0.0
-}
insertToTableReturning ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity ReturningClause
insertToTableReturning =
  insertTable WithReturning

-- an internal helper function for creating an insert with a given `ReturningOption`
insertTable ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity returningClause
insertTable returningOption tableDef entities =
  rawInsertExpr returningOption (tableMarshaller tableDef) (mkInsertExpr returningOption tableDef entities)

{- |
  Builds an 'Insert' that will execute the specified query and use the given 'SqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.InsertExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'SqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Update'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.InsertExpr'. It is expected that the 'ReturningOption' given matches the
  'Expr.InsertExpr'. This level of interface does not provide an automatic enforcement of the
  expectation, however failure is likely if that is not met.

@since 1.0.0.0
-}
rawInsertExpr :: ReturningOption returningClause -> AnnotatedSqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity returningClause
rawInsertExpr WithReturning = InsertReturning
rawInsertExpr WithoutReturning = Insert
