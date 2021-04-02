module StrangeFieldNames.Entity where

import Data.Int (Int32)

import qualified Database.Orville.PostgreSQL as O

-- field is generic and is meant to be used with different column names to test
-- that crud actions work with various name styles Ex. camelCase, snake_case etc.
data CrudEntity key = CrudEntity
  { crudEntityId :: key
  , crudEntityField :: Int32
  } deriving (Show, Eq)

newtype CrudEntityId = CrudEntityId
  { unCrudEntityId :: Int32
  } deriving (Show, Eq)

-- Take in the column name so we can test different styles.
crudEntityTable ::
  String ->
  O.TableDefinition (CrudEntity CrudEntityId) (CrudEntity ()) CrudEntityId
crudEntityTable columnName =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "crudEntity"
    , O.tblPrimaryKey = O.primaryKey crudEntityIdField
    , O.tblMapper =
      CrudEntity
        <$> O.readOnlyField crudEntityIdField
        <*> O.attrField crudEntityField (crudEntityRenameableField columnName)
    , O.tblGetKey = crudEntityId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

crudEntityIdField :: O.FieldDefinition O.NotNull CrudEntityId
crudEntityIdField =
  O.automaticIdField "id" `O.withConversion`
  O.convertSqlType unCrudEntityId CrudEntityId

crudEntityRenameableField :: String -> O.FieldDefinition O.NotNull Int32
crudEntityRenameableField = O.int32Field

