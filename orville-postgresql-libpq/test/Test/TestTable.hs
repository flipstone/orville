module Test.TestTable
  ( dropAndRecreateTableDef,
    dropTableDef,
    dropTableDefSql,
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
dropTableDefSql tableDef = do
  RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql (tableName tableDef)
