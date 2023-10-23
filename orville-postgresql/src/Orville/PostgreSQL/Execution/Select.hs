{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Functions for working with executable @SELECT@ statements. The 'Select' type is
a value that can be passed around and executed later. The 'Select' is directly
associated with how to decode the rows returned by the query. This means it
can be safely executed via 'executeSelect' and used to decode the rows. It is a
lower-level API than the entity select functions in
"Orville.PostgreSQL.Execution.EntityOperations", but not as primitive as
"Orville.PostgreSQL.Expr.Query".

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.Select
  ( Select
  , executeSelect
  , useSelect
  , selectToQueryExpr
  , selectTable
  , selectMarshalledColumns
  , rawSelectQueryExpr
  )
where

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, marshallerDerivedColumns, unannotatedSqlMarshaller)
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (TableDefinition, tableMarshaller, tableName)

{- |
  Represents a @SELECT@ statement that can be executed against a database. A
  'Select' has a 'SqlMarshaller' bound to it that will be used to decode the
  database result set when it is executed.

@since 1.0.0.0
-}
data Select readEntity where
  Select :: AnnotatedSqlMarshaller writeEntity readEntity -> Expr.QueryExpr -> Select readEntity

{- |
  Extracts the query that will be run when the select is executed. Normally you
  don't want to extract the query and run it yourself, but this function is
  useful to view the query for debugging or query explanation.

@since 1.0.0.0
-}
selectToQueryExpr :: Select readEntity -> Expr.QueryExpr
selectToQueryExpr (Select _ query) = query

{- |
  Executes the database query for the 'Select' and uses its 'SqlMarshaller' to
  decode the result set.

@since 1.0.0.0
-}
executeSelect :: Monad.MonadOrville m => Select row -> m [row]
executeSelect =
  useSelect (Execute.executeAndDecode QueryType.SelectQuery)

{- |
  Runs a function allowing it to use the data elements containted within the
  'Select' it pleases. The marshaller that the function is provided can be use
  to decode results from the query.

@since 1.0.0.0
-}
useSelect ::
  ( forall writeEntity.
    Expr.QueryExpr ->
    AnnotatedSqlMarshaller writeEntity readEntity ->
    a
  ) ->
  Select readEntity ->
  a
useSelect f (Select marshaller query) =
  f query marshaller

{- |
  Builds a 'Select' that will select all the columns described in the
  'TableDefinition'. This is the safest way to build a 'Select', because table
  name and columns are all read from the 'TableDefinition'. If the table is
  being managed with Orville auto-migrations, this will match the schema in the
  database.

@since 1.0.0.0
-}
selectTable ::
  TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  Select readEntity
selectTable tableDef =
  selectMarshalledColumns (tableMarshaller tableDef) (tableName tableDef)

{- |
  Builds a 'Select' that will select the columns described by the marshaller
  from the specified table. It is up to the caller to ensure that the columns
  in the marshaller make sense for the table.

  This function is useful for querying a subset of table columns using a custom
  marshaller.

@since 1.0.0.0
-}
selectMarshalledColumns ::
  AnnotatedSqlMarshaller writeEntity readEntity ->
  Expr.Qualified Expr.TableName ->
  SelectOptions.SelectOptions ->
  Select readEntity
selectMarshalledColumns marshaller qualifiedTableName selectOptions =
  rawSelectQueryExpr marshaller $
    SelectOptions.selectOptionsQueryExpr
      (Expr.selectDerivedColumns (marshallerDerivedColumns . unannotatedSqlMarshaller $ marshaller))
      (Expr.referencesTable qualifiedTableName)
      selectOptions

{- |
  Builds a 'Select' that will execute the specified query and use the given
  'SqlMarshaller' to decode it. It is up to the caller to ensure that the given
  'Expr.QueryExpr' makes sense and produces a result set that the
  'SqlMarshaller' can decode.

  This is the lowest level of escape hatch available for 'Select'. The caller
  can build any query that Orville supports using the expression-building
  functions, or use @RawSql.fromRawSql@ to build a raw 'Expr.QueryExpr'.

@since 1.0.0.0
-}
rawSelectQueryExpr ::
  AnnotatedSqlMarshaller writeEntity readEntity ->
  Expr.QueryExpr ->
  Select readEntity
rawSelectQueryExpr = Select
