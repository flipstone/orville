{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.InformationSchema.TableName
  ( TableName (..),
    tableNameFromText,
    tableNameField,
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  The @table_name@ field of the @information_schema@ tables.
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
