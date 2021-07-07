module Test.TestTable
  ( dropAndRecreateTableDef,
  )
where

import Orville.PostgreSQL.Connection (Connection)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.TableDefinition (TableDefinition, mkCreateTableExpr, tableName)

dropAndRecreateTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropAndRecreateTableDef connection tableDef = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql (tableName tableDef))
  RawSql.executeVoid connection (RawSql.toRawSql $ mkCreateTableExpr tableDef)
