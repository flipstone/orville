module Orville.PostgreSQL.InformationSchema.InformationSchemaTable
  ( InformationSchemaTable (..),
    informationSchemaTablesTable,
    tableCatalogField,
    tableSchemaField,
    tableNameField,
  )
where

import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  Contains metadata about a table collected from the @information_schema@ table in
  the database.
-}
data InformationSchemaTable = InformationSchemaTable
  { tableCatalog :: T.Text
  , tableSchema :: T.Text
  , tableName :: T.Text
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
    <$> Orville.marshallField tableCatalog (Orville.unboundedTextField "table_catalog")
    <*> Orville.marshallField tableSchema (Orville.unboundedTextField "table_schema")
    <*> Orville.marshallField tableName (Orville.unboundedTextField "table_name")

{- |
  The @table_catalog@ field of the @information_schema.tables@ table.
-}
tableCatalogField :: Orville.FieldDefinition Orville.NotNull T.Text
tableCatalogField = Orville.unboundedTextField "table_catalog"

{- |
  The @table_schema@ field of the @information_schema.tables@ table.
-}
tableSchemaField :: Orville.FieldDefinition Orville.NotNull T.Text
tableSchemaField = Orville.unboundedTextField "table_schema"

{- |
  The @table_name@ field of the @information_schema.tables@ table.
-}
tableNameField :: Orville.FieldDefinition Orville.NotNull T.Text
tableNameField = Orville.unboundedTextField "table_name"
