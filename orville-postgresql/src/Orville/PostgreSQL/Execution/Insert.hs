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
  , upsertToTable
  , upsertToTableReturning
  , rawInsertExpr
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Batchable (Batchable)
import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import Orville.PostgreSQL.Execution.ReturningOption (NoReturningClause, ReturningClause, ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (TableDefinition, mkInsertExpr, tableMarshaller)

{- | Represents an @INSERT@ statement that can be executed against a database. An
'Insert' has a 'Orville.PostgreSQL.SqlMarshaller' bound to it that, when the
insert returns data from the database, will be used to decode the database
result set when it is executed.

@since 1.0.0.0
-}
data Insert readEntity returningClause where
  Insert ::
    Marshall.AnnotatedSqlMarshaller writeEntity readEntity ->
    Expr.InsertExpr ->
    Insert readEntity NoReturningClause
  InsertReturning ::
    Marshall.AnnotatedSqlMarshaller writeEntity readEntity ->
    Expr.InsertExpr ->
    Insert readEntity ReturningClause

{- | Extracts the query that will be run when the insert is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.

@since 1.0.0.0
-}
insertToInsertExpr :: Insert readEntity returningClause -> Expr.InsertExpr
insertToInsertExpr (Insert _ expr) = expr
insertToInsertExpr (InsertReturning _ expr) = expr

{- | Executes the database query for the 'Insert' and returns the number of rows
  affected by the query.

@since 1.0.0.0
-}
executeInsert ::
  Monad.MonadOrville m =>
  Insert readEntity NoReturningClause ->
  m Int
executeInsert (Insert _ expr) =
  Execute.executeAndReturnAffectedRows QueryType.InsertQuery expr

{- | Executes the database query for the 'Insert' and uses its
'Orville.PostgreSQL.SqlMarshaller' to decode the rows (that were just inserted)
as returned via a RETURNING clause.

@since 1.0.0.0
-}
executeInsertReturnEntities ::
  Monad.MonadOrville m =>
  Insert readEntity ReturningClause ->
  m [readEntity]
executeInsertReturnEntities (InsertReturning marshaller expr) =
  Execute.executeAndDecode QueryType.InsertQuery expr marshaller

{- | Builds an 'Insert' that will insert all of the writable columns described in the
  'TableDefinition' without returning the data as seen by the database.

@since 1.0.0.0
-}
insertToTable ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Batchable (Insert readEntity NoReturningClause)
insertToTable =
  insertTable WithoutReturning

{- | Builds an 'Insert' that will insert all of the writable columns described in the
  'TableDefinition' and return the data as seen by the database. This is useful for getting
  database-managed columns such as auto-incrementing identifiers and sequences.

@since 1.0.0.0
-}
insertToTableReturning ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Batchable (Insert readEntity ReturningClause)
insertToTableReturning =
  insertTable WithReturning

{- | Builds an 'Insert' with an @ON CONFLICT@ clause that will insert new rows, or update conflicting rows
  using a @SET@ clause derived from the writable columns of the table's @SqlMarshaller@.

@since 1.1.1.0.1
-}
upsertToTable ::
  TableDefinition key writeEntity readEntity ->
  Expr.ConflictTargetExpr ->
  NonEmpty writeEntity ->
  Batchable (Insert readEntity NoReturningClause)
upsertToTable =
  upsertTable WithoutReturning

{- | Builds an 'Insert' with an @ON CONFLICT@ clause that will insert new rows, or update conflicting rows
  using a @SET@ clause derived from the writable columns of the table's @SqlMarshaller@. Returns the inserted
  and updated rows.

@since 1.1.1.0.1
-}
upsertToTableReturning ::
  TableDefinition key writeEntity readEntity ->
  Expr.ConflictTargetExpr ->
  NonEmpty writeEntity ->
  Batchable (Insert readEntity ReturningClause)
upsertToTableReturning =
  upsertTable WithReturning

-- an internal helper function for creating an insert with a given `ReturningOption`
insertTable ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Batchable (Insert readEntity returningClause)
insertTable returningOption tableDef entities =
  fmap
    (rawInsertExpr returningOption (tableMarshaller tableDef))
    (mkInsertExpr returningOption tableDef Nothing entities)

-- an internal helper function for creating an @insert ... on conflict <target> do update set ...@ with a given `ReturningOption`
upsertTable ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  Expr.ConflictTargetExpr ->
  NE.NonEmpty writeEntity ->
  Batchable (Insert readEntity returningClause)
upsertTable returningOption tableDef target entities =
  let
    marshaller = tableMarshaller tableDef
    unAnnTableMarshaller = Marshall.unannotatedSqlMarshaller marshaller

    conflictSetItemExprs =
      Marshall.foldMarshallerFields
        unAnnTableMarshaller
        []
        ( Marshall.collectFromField
            Marshall.ExcludeReadOnlyColumns
            (const $ Expr.setColumnNameExcluded . Expr.unqualified . Marshall.fieldColumnName)
        )

    mkOnConflictExpr neSetItemExprs =
      Expr.onConflictDoUpdate (Just target) neSetItemExprs Nothing
  in
    fmap
      (rawInsertExpr returningOption marshaller)
      ( mkInsertExpr
          returningOption
          tableDef
          (fmap mkOnConflictExpr (NE.nonEmpty conflictSetItemExprs))
          entities
      )

{- | Builds an 'Insert' that will execute the specified query and use the given
  'Orville.PostgreSQL.SqlMarshaller' to decode it. It is up to the caller to
  ensure that the given 'Expr.InsertExpr' makes sense and produces a value that
  can be stored, as well as returning a result that the
  'Orville.PostgreSQL.SqlMarshaller' can decode.

  This is the lowest level of escape hatch available for 'Insert'. The caller can build any query
  that Orville supports using the expression-building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.InsertExpr'. It is expected that the 'ReturningOption' given matches the
  'Expr.InsertExpr'. This level of interface does not provide an automatic enforcement of the
  expectation, however failure is likely if that is not met.

@since 1.0.0.0
-}
rawInsertExpr ::
  ReturningOption returningClause ->
  Marshall.AnnotatedSqlMarshaller writeEntity readEntity ->
  Expr.InsertExpr ->
  Insert readEntity returningClause
rawInsertExpr WithReturning = InsertReturning
rawInsertExpr WithoutReturning = Insert
