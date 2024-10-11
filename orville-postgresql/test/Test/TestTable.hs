module Test.TestTable
  ( dropAndRecreateTableDef
  , dropTableDef
  , dropTableDefSql
  , dropTableNameSql
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Raw.Connection (Connection)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Schema (TableDefinition, mkCreateTableExpr, tableName)

dropTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropTableDef connection tableDef = do
  RawSql.executeVoid connection (dropTableDefSql tableDef)

dropAndRecreateTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropAndRecreateTableDef connection tableDef = do
  dropTableDef connection tableDef
  RawSql.executeVoid connection (mkCreateTableExpr tableDef)

dropTableDefSql ::
  TableDefinition key writeEntity readEntity ->
  RawSql.RawSql
dropTableDefSql =
  dropTableNameExprSql . tableName

dropTableNameSql ::
  String ->
  RawSql.RawSql
dropTableNameSql =
  dropTableNameExprSql . Expr.unqualified . Expr.tableName

dropTableNameExprSql ::
  Expr.QualifiedOrUnqualified Expr.TableName ->
  RawSql.RawSql
dropTableNameExprSql name =
  RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql name
