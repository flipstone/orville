{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.InformationSchema.TableCatalog
  ( CatalogName (..),
    catalogNameFromText,
    tableCatalogField,
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- |
  The @table_catalog@ field of the @information_schema@ tables.
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
