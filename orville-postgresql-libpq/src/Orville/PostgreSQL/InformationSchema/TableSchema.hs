{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.InformationSchema.TableSchema
  ( SchemaName (..),
    schemaNameFromText,
    tableSchemaField,
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  The @table_schema@ field of the @information_schema@ tables.
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
