{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgExtension
  ( PgExtension (..)
  , ExtensionName
  , pgExtensionTable
  , extensionNameField
  )
where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville

{- | The Haskell representation of data read from the @pg_catalog.pg_extension@ table.
  Rows in this table contain extended information about extensions.

@since 1.1.0.0
-}
newtype PgExtension = PgExtension
  { pgExtensionName :: ExtensionName
  -- ^ The PostgreSQL name for this extension.
  }

{- | A Haskell type for the name of the extension represented by a 'PgExtension'.

@since 1.1.0.0
-}
newtype ExtensionName
  = ExtensionName T.Text
  deriving
    ( -- | @since 1.1.0.0
      Show
    , -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    , -- | @since 1.1.0.0
      String.IsString
    )

{- | An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_extension@ table.

@since 1.0.0.0
-}
pgExtensionTable :: Orville.TableDefinition Orville.NoKey PgExtension PgExtension
pgExtensionTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_extension"
      pgExtensionMarshaller

pgExtensionMarshaller :: Orville.SqlMarshaller PgExtension PgExtension
pgExtensionMarshaller =
  PgExtension
    <$> Orville.marshallField pgExtensionName extensionNameField

{- | The @extname@ column of the @pg_extension@ table.

@since 1.1.0.0
-}
extensionNameField :: Orville.FieldDefinition Orville.NotNull ExtensionName
extensionNameField =
  Orville.coerceField $
    Orville.boundedTextField "extname" 63
