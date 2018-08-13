{-|
Module    : Database.Orville.Internal.FieldDefintion
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Internal.FieldDefinition where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.HDBC

import Database.Orville.Internal.Expr.NameExpr (NameForm(..))
import Database.Orville.Internal.SqlConversion
import Database.Orville.Internal.Types

textField :: String -> Int -> FieldDefinition Text
textField name len = FieldDefinition name (VarText len) [] textConversion

fixedTextField :: String -> Int -> FieldDefinition Text
fixedTextField name len = FieldDefinition name (Text len) [] textConversion

dayField :: String -> FieldDefinition Day
dayField = fieldOfType Date dayConversion

utcTimeField :: String -> FieldDefinition UTCTime
utcTimeField = fieldOfType Timestamp utcTimeConversion

int32Field :: String -> FieldDefinition Int32
int32Field = fieldOfType Integer int32Conversion

int64Field :: String -> FieldDefinition Int64
int64Field = fieldOfType BigInteger int64Conversion

doubleField :: String -> FieldDefinition Double
doubleField = fieldOfType Double doubleConversion

boolField :: String -> FieldDefinition Bool
boolField = fieldOfType Boolean boolConversion

automaticIdField :: String -> FieldDefinition Int
automaticIdField = fieldOfType AutomaticId intConversion

searchVectorField :: String -> FieldDefinition Text
searchVectorField = fieldOfType TextSearchVector textConversion

nullableField :: FieldDefinition a -> FieldDefinition (Maybe a)
nullableField field = field `withFlag` Null `withConversion` nullableConversion

foreignKeyField ::
     String
  -> TableDefinition readEntity writeEntity key
  -> FieldDefinition key
  -> FieldDefinition key
foreignKeyField name refTable refField =
  FieldDefinition
    name
    (foreignFieldType $ fieldType refField)
    [References refTable refField]
    (fieldConversion refField)
  where
    foreignFieldType AutomaticId = ForeignId
    foreignFieldType typ = typ

-- This is an internal field for building the basic field types
-- above. It should not be exposed outside Orville
fieldOfType :: ColumnType -> SqlConversion a -> String -> FieldDefinition a
fieldOfType columnType conversion name = FieldDefinition name columnType [] conversion

isPrimaryKey :: ColumnFlag -> Bool
isPrimaryKey PrimaryKey = True
isPrimaryKey _ = False

isNullFlag :: ColumnFlag -> Bool
isNullFlag Null = True
isNullFlag _ = False

isAssignedByDatabase :: ColumnFlag -> Bool
isAssignedByDatabase AssignedByDatabase = True
isAssignedByDatabase _ = False

escapedFieldName :: FieldDefinition a -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

isPrimaryKeyField :: FieldDefinition a -> Bool
isPrimaryKeyField field = any isPrimaryKey $ fieldFlags field

withFlag :: FieldDefinition a -> ColumnFlag -> FieldDefinition a
withFlag field newFlag = field { fieldFlags = newFlag : fieldFlags field }

withName :: FieldDefinition a -> String -> FieldDefinition a
withName field newName = field { fieldName = newName }

withConversion ::
     FieldDefinition a
  -> (SqlConversion a -> SqlConversion b)
  -> FieldDefinition b
withConversion field mapConversion = field { fieldConversion = mapConversion $ fieldConversion field }

isAssignedByDatabaseField :: FieldDefinition a -> Bool
isAssignedByDatabaseField field = any isAssignedByDatabase $ fieldFlags field

withPrefix :: FieldDefinition a -> String -> FieldDefinition a
withPrefix field prefix = field `withName` (prefix ++ "_" ++ fieldName field)

fieldToNameForm :: FieldDefinition a -> NameForm
fieldToNameForm field = NameForm Nothing (fieldName field)

fieldToSqlValue :: FieldDefinition a -> a -> SqlValue
fieldToSqlValue = convertToSql . fieldConversion

fieldFromSqlValue :: FieldDefinition a -> SqlValue -> Maybe a
fieldFromSqlValue = convertFromSql . fieldConversion
