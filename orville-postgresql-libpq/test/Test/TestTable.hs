module Test.TestTable
  ( dropAndRecreateTableDef,
  )
where

import Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.TableDefinition (TableDefinition (tableName), mkCreateTableExpr)

dropAndRecreateTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropAndRecreateTableDef connection tableDef = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql (tableName tableDef))
  RawSql.executeVoid connection (Expr.createTableExprToSql $ mkCreateTableExpr tableDef)
