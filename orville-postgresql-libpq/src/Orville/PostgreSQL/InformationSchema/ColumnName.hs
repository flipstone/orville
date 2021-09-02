{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.InformationSchema.ColumnName
  ( ColumnName (..),
    columnNameFromText,
    columnNameField,
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  The @column_name@ field of the @information_schema@ tables.
-}
columnNameField :: Orville.FieldDefinition Orville.NotNull ColumnName
columnNameField =
  Orville.coerceField
    (Orville.unboundedTextField "column_name")

{- |
  Represents a column name as data retrieved from @information_schema@ tables,
  not as it would be used in a SQL expression.
-}
newtype ColumnName
  = ColumnName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Create a 'ColumnName' from a 'T.Text' value
-}
columnNameFromText :: T.Text -> ColumnName
columnNameFromText = ColumnName
