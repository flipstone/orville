module Orville.PostgreSQL.InformationSchema.InformationSchemaTable
  ( InformationSchemaTable (..),
    informationSchemaTablesTable,
  )
where

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.InformationSchema.TableCatalog (CatalogName, tableCatalogField)
import Orville.PostgreSQL.InformationSchema.TableName (TableName, tableNameField)
import Orville.PostgreSQL.InformationSchema.TableSchema (SchemaName, tableSchemaField)

{- |
  Contains metadata about a table collected from the @information_schema.tables@ table in
  the database.
-}
data InformationSchemaTable = InformationSchemaTable
  { tableCatalog :: CatalogName
  , tableSchema :: SchemaName
  , tableName :: TableName
  }
  deriving (Show, Eq)

{- |
  An Orville 'Orville.TableDefinition' that can be used to query the
  @information_schema.tables@ table to gather metadata about the tables in the
  database.
-}
informationSchemaTablesTable :: Orville.TableDefinition Orville.NoKey InformationSchemaTable InformationSchemaTable
informationSchemaTablesTable =
  Orville.setTableSchema "information_schema" $
    Orville.mkTableDefinitionWithoutKey
      "tables"
      informationSchemaTableMarshaller

informationSchemaTableMarshaller :: Orville.SqlMarshaller InformationSchemaTable InformationSchemaTable
informationSchemaTableMarshaller =
  InformationSchemaTable
    <$> Orville.marshallField tableCatalog tableCatalogField
    <*> Orville.marshallField tableSchema tableSchemaField
    <*> Orville.marshallField tableName tableNameField
