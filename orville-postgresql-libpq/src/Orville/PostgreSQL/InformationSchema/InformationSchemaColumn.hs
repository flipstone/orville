module Orville.PostgreSQL.InformationSchema.InformationSchemaColumn
  ( InformationSchemaColumn (..),
    informationSchemaColumnsTable,
    describeTableColumns,
  )
where

import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.InformationSchema.ColumnName (ColumnName, columnNameField)
import Orville.PostgreSQL.InformationSchema.TableCatalog (CatalogName, tableCatalogField)
import Orville.PostgreSQL.InformationSchema.TableName (TableName, tableNameField)
import Orville.PostgreSQL.InformationSchema.TableSchema (SchemaName, tableSchemaField)
import Orville.PostgreSQL.InformationSchema.InformationSchemaTable (InformationSchemaTable(tableCatalog, tableSchema, tableName))

{- |
  Contains metadata about a column collected from the @information_schema.columns@ table in
  the database.
-}
data InformationSchemaColumn = InformationSchemaColumn
  { columnTableCatalog :: CatalogName
  , columnTableSchema :: SchemaName
  , columnTableName :: TableName
  , columnName :: ColumnName
  }
  deriving (Show, Eq)

{- |
  An Orville 'Orville.TableDefinition' that can be used to query the
  @information_schema.column@ table to gather metadata about the tables in the
  database.
-}
informationSchemaColumnsTable :: Orville.TableDefinition Orville.NoKey InformationSchemaColumn InformationSchemaColumn
informationSchemaColumnsTable =
  Orville.setTableSchema "information_schema" $
    Orville.mkTableDefinitionWithoutKey
      "columns"
      informationSchemaColumnMarshaller

informationSchemaColumnMarshaller :: Orville.SqlMarshaller InformationSchemaColumn InformationSchemaColumn
informationSchemaColumnMarshaller =
  InformationSchemaColumn
    <$> Orville.marshallField columnTableCatalog tableCatalogField
    <*> Orville.marshallField columnTableSchema tableSchemaField
    <*> Orville.marshallField columnTableName tableNameField
    <*> Orville.marshallField columnName columnNameField

{- |
  Retrieves the @information_schema@ column information for the given table.
-}
describeTableColumns :: Orville.MonadOrville m
                     => InformationSchemaTable
                     -> m [InformationSchemaColumn]
describeTableColumns table =
  Orville.findEntitiesBy
    informationSchemaColumnsTable
    (Orville.where_
      (Orville.whereAnd
        (Orville.fieldEquals tableCatalogField (tableCatalog table)
        :| [ Orville.fieldEquals tableSchemaField (tableSchema table)
           , Orville.fieldEquals tableNameField (tableName table)
           ]
        )
      )
    )
