module QuotedFields.Schema.Entity
( nonNullableEntityTable
, nullableEntityTable
, droppedEntityTable
, snakeCaseField
, camelCaseField
, orderField
)where

import Data.Int (Int32)
import qualified Database.Orville.PostgreSQL as O

import QuotedFields.Data.Entity (NullableEntity(..), NonNullableEntity(..), DroppedEntity(..))



nonNullableEntityTable :: O.TableDefinition NonNullableEntity NonNullableEntity Int32
nonNullableEntityTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "entity"
    , O.tblPrimaryKey = snakeCaseField
    , O.tblMapper =
      NonNullableEntity
        <$> O.attrField nonNullSnakeCase snakeCaseField
        <*> O.attrField nonNullCamelCase camelCaseField
        <*> O.attrField nonNullOrder orderField
    , O.tblGetKey = nonNullSnakeCase
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

nullableEntityTable :: O.TableDefinition NullableEntity NullableEntity Int32
nullableEntityTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "entity"
    , O.tblPrimaryKey = snakeCaseField
    , O.tblMapper =
      NullableEntity
        <$> O.attrField nullableSnakeCase snakeCaseField
        <*> O.attrField nullableCamelCase (O.nullableField camelCaseField)
        <*> O.attrField nullableOrder (O.nullableField orderField)
    , O.tblGetKey = nullableSnakeCase
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

droppedEntityTable :: O.TableDefinition DroppedEntity DroppedEntity Int32
droppedEntityTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "entity"
    , O.tblPrimaryKey = snakeCaseField
    , O.tblMapper = DroppedEntity <$> O.attrField droppedSnakeCase snakeCaseField
    , O.tblGetKey = droppedSnakeCase
    , O.tblSafeToDelete = ["camelCase", "order"]
    , O.tblComments = O.noComments
    }

snakeCaseField :: O.FieldDefinition Int32
snakeCaseField = O.int32Field "snake_case" `O.withFlag` O.PrimaryKey

camelCaseField :: O.FieldDefinition Int32
camelCaseField = O.int32Field "camelCase"

-- "order" is a reserverd word and must be surrounded in quotes when using it as
-- a column name.
orderField :: O.FieldDefinition Int32
orderField = O.int32Field "order"

