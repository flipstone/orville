module Migrations.Entity where

import Data.Int (Int32)

import qualified Database.Orville.PostgreSQL as O

-- This data type is paramaterized to allow for different types of migrations
-- Ex. Adding a field, changing a field to nullable, etc.
-- The field is used for multiple columns to test that migrations work with
-- various name styles Ex. camelCase, snake_case etc.
data MigrationEntity field key = MigrationEntity
  { migrationEntityId :: key
  , field :: field
  }

newtype MigrationEntityId = MigrationEntityId
  { unMigrationEntityId :: Int32
  } deriving (Show, Eq)

-- Take in the field definition so we can force different names and field types
-- during testing.
migrationEntityTable ::
  O.FieldDefinition nullability a ->
  O.TableDefinition (MigrationEntity a MigrationEntityId) (MigrationEntity a ()) MigrationEntityId
migrationEntityTable fieldDef =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "migrationEntity"
    , O.tblPrimaryKey = O.primaryKey migrationEntityIdField
    , O.tblMapper =
      MigrationEntity
        <$> O.readOnlyField migrationEntityIdField
        <*> O.attrField field fieldDef
    , O.tblGetKey = migrationEntityId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

-- Same as migrationEntityTable but with the given column name dropped.
migrationEntityTableWithDroppedColumn ::
  String ->
  O.TableDefinition (MigrationEntity () MigrationEntityId) (MigrationEntity () ()) MigrationEntityId
migrationEntityTableWithDroppedColumn columnName =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "migrationEntity"
    , O.tblPrimaryKey = O.primaryKey migrationEntityIdField
    , O.tblMapper =
      MigrationEntity
        <$> O.readOnlyField migrationEntityIdField
        <*> pure ()
    , O.tblGetKey = migrationEntityId
    , O.tblSafeToDelete = [columnName]
    , O.tblComments = O.noComments
    }

migrationEntityIdField :: O.FieldDefinition O.NotNull MigrationEntityId
migrationEntityIdField =
  O.automaticIdField "id" `O.withConversion`
  O.convertSqlType unMigrationEntityId MigrationEntityId

