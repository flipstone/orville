{-# LANGUAGE GADTs #-}

{- |

Module    : Orville.PostgreSQL.Internal.Insert
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Insert (Insert, insertToInsertExpr, executeInsert, executeInsertReturnEntities, insertToTableReturning, insertToTable) where

import Data.List.NonEmpty (NonEmpty)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (ReturningOption (WithReturning, WithoutReturning), TableDefinition, mkInsertExpr, tableMarshaller)

{- | Represents an @INSERT@ statement that can be executed against a database. An 'Insert' has a
  'SqlMarshaller' bound to it that, when the insert returns data from the database, will be used to
  decode the database result set when it is executed.
-}
data Insert readEntity where
  Insert :: SqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity

{- |
  Extracts the query that will be run when the insert is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.
-}
insertToInsertExpr :: Insert readEntity -> Expr.InsertExpr
insertToInsertExpr (Insert _ expr) = expr

{- |
  Excutes the database query for the 'Insert' and returns '()'.
-}
executeInsert :: MonadOrville.MonadOrville m => Insert readEntity -> m ()
executeInsert (Insert _ expr) =
  Execute.executeVoid expr

{- |
  Excutes the database query for the 'Insert' and uses its 'SqlMarshaller' to
  decode the result set.
-}
executeInsertReturnEntities :: MonadOrville.MonadOrville m => Insert readEntity -> m [readEntity]
executeInsertReturnEntities (Insert marshaller expr) =
  Execute.executeAndDecode expr marshaller

{- |
  Builds an 'Insert' that will insert all of the writeable columns described in the
  'TableDefinition' without returning the data as seen by the database.
-}
insertToTable ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertToTable =
  insertTable WithoutReturning

{- |
  Builds an 'Insert' that will insert all of the writeable columns described in the
  'TableDefinition' and returning the data as seen by the database. This is useful for getting
  database managed columns such as auto-incrementing identifiers and sequences.
-}
insertToTableReturning ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertToTableReturning =
  insertTable WithReturning

-- an internal helper function for creating an insert with a given `ReturningOption`
insertTable ::
  ReturningOption ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertTable returningOption tableDef entities =
  rawInsertExpr (tableMarshaller tableDef) (mkInsertExpr returningOption tableDef entities)

{- |
  Builds an 'Insert' that will execute the specified query and use the given 'SqlMarshaller' to
  decode it. It is up to the caller to ensure that the given 'Expr.QueryExpr' makes sense and
  produces a value that can be stored, as well as returning a result that the 'SqlMarshaller' can
  decode.

  This is the lowest level of escape hatch available for 'Select'. The caller can build any query
  that Orville supports using the expression building functions, or use @RawSql.fromRawSql@ to build
  a raw 'Expr.QueryExpr'.
-}
rawInsertExpr :: SqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity
rawInsertExpr = Insert
