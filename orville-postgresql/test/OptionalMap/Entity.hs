module OptionalMap.Entity where

import Data.Int (Int32)
import qualified Data.Text as T

import qualified Database.Orville.PostgreSQL as O

data CrudEntity key = CrudEntity
  { crudEntityId :: key
  , crudEntityComplexField :: Maybe ComplexField
  } deriving (Show, Eq)

newtype CrudEntityId = CrudEntityId
  { unCrudEntityId :: Int32
  } deriving (Show, Eq)

data ComplexField =
  ComplexField { complexFieldPart1 :: Int32
               , complexFieldPart2 :: Maybe T.Text
               } deriving (Show, Eq)

-- Take in the column name so we can test different styles.
badCrudEntityTable ::
  O.TableDefinition (CrudEntity CrudEntityId) (CrudEntity ()) CrudEntityId
badCrudEntityTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "crudEntity"
    , O.tblPrimaryKey = crudEntityIdField
    , O.tblMapper =
      CrudEntity
        <$> O.readOnlyField crudEntityIdField
        <*> O.mapAttr crudEntityComplexField (O.maybeMapper badComplexFieldMap)
    , O.tblGetKey = crudEntityId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

crudEntityIdField :: O.FieldDefinition CrudEntityId
crudEntityIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unCrudEntityId CrudEntityId

badComplexFieldMap :: O.RelationalMap ComplexField ComplexField
badComplexFieldMap =
  ComplexField <$> O.attrField complexFieldPart1 complexFieldPart1Field
               <*> O.attrField complexFieldPart2 (O.nullableField complexFieldPart2Field)

complexFieldPart1Field :: O.FieldDefinition Int32
complexFieldPart1Field = O.int32Field "part_1"

complexFieldPart2Field :: O.FieldDefinition T.Text
complexFieldPart2Field =
  O.withConversion (O.textField "part_2" 255) $ O.maybeConvertSqlType id Just
