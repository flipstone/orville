{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.InformationSchema.InformationSchemaTable
  ( InformationSchemaTable (..),
    informationSchemaTablesTable,
    CatalogName (..),
    catalogNameFromText,
    tableCatalogField,
    SchemaName (..),
    schemaNameFromText,
    tableSchemaField,
    TableName (..),
    tableNameFromText,
    tableNameField,
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  Contains metadata about a table collected from the @information_schema@ table in
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

{- |
  The @table_catalog@ field of the @information_schema.tables@ table.
-}
tableCatalogField :: Orville.FieldDefinition Orville.NotNull CatalogName
tableCatalogField =
  Orville.coerceField
    (Orville.unboundedTextField "table_catalog")

{- |
  Represents a schema name as data retrieved from @information_schema@ tables,
  not as it would be used in a SQL expression.
-}
newtype CatalogName
  = CatalogName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Create a 'CatalogName' from a 'T.Text' value
-}
catalogNameFromText :: T.Text -> CatalogName
catalogNameFromText = CatalogName

{- |
  The @table_schema@ field of the @information_schema.tables@ table.
-}
tableSchemaField :: Orville.FieldDefinition Orville.NotNull SchemaName
tableSchemaField =
  Orville.coerceField
    (Orville.unboundedTextField "table_schema")

{- |
  Represents a schema name as data retrieved from @information_schema@ tables,
  not as it would be used in a SQL expression.
-}
newtype SchemaName
  = SchemaName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Create a 'SchemaName' from a 'T.Text' value
-}
schemaNameFromText :: T.Text -> SchemaName
schemaNameFromText = SchemaName

{- |
  The @table_name@ field of the @information_schema.tables@ table.
-}
tableNameField :: Orville.FieldDefinition Orville.NotNull TableName
tableNameField =
  Orville.coerceField
    (Orville.unboundedTextField "table_name")

{- |
  Represents a table name as data retrieved from @information_schema@ tables,
  not as it would be used in a SQL expression.
-}
newtype TableName
  = TableName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Create a 'TableName' from a 'T.Text' value
-}
tableNameFromText :: T.Text -> TableName
tableNameFromText = TableName
