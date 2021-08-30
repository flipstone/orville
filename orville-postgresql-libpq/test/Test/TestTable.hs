module Test.TestTable
  ( dropAndRecreateTableDef,
    dropTableDef,
  )
where

import Orville.PostgreSQL.Connection (Connection)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.TableDefinition (TableDefinition, mkCreateTableExpr, tableName)

dropTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropTableDef connection tableDef = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql (tableName tableDef))

dropAndRecreateTableDef ::
  Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropAndRecreateTableDef connection tableDef = do
  dropTableDef connection tableDef
  RawSql.executeVoid connection (mkCreateTableExpr tableDef)
