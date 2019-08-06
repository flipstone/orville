{-|
Module    : Database.Orville.PostgreSQL.Internal.FieldDefintion
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.FieldDefinition where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.HDBC

import Database.Orville.PostgreSQL.Internal.Expr.NameExpr (NameForm(..))
import Database.Orville.PostgreSQL.Internal.SqlType
import Database.Orville.PostgreSQL.Internal.Types

textField :: String -> Int -> FieldDefinition Text
textField name len = FieldDefinition name (varText len) []

fixedTextField :: String -> Int -> FieldDefinition Text
fixedTextField name len = FieldDefinition name (text len) []

dayField :: String -> FieldDefinition Day
dayField = fieldOfType date

utcTimeField :: String -> FieldDefinition UTCTime
utcTimeField = fieldOfType timestamp

int32Field :: String -> FieldDefinition Int32
int32Field = fieldOfType integer

int64Field :: String -> FieldDefinition Int64
int64Field = fieldOfType bigInteger

doubleField :: String -> FieldDefinition Double
doubleField = fieldOfType double

boolField :: String -> FieldDefinition Bool
boolField = fieldOfType boolean

automaticIdField :: String -> FieldDefinition Int32
automaticIdField = fieldOfType serial

searchVectorField :: String -> FieldDefinition Text
searchVectorField = fieldOfType textSearchVector

nullableField :: FieldDefinition a -> FieldDefinition (Maybe a)
nullableField field = field `withConversion` nullableType

foreignKeyField ::
     String
  -> TableDefinition readEntity writeEntity key
  -> FieldDefinition key
  -> FieldDefinition key
foreignKeyField name refTable refField =
  FieldDefinition
    name
    (foreignRefType $ fieldType refField)
    [References refTable refField]

fieldOfType :: SqlType a -> String -> FieldDefinition a
fieldOfType sqlType name = FieldDefinition name sqlType []

isPrimaryKey :: ColumnFlag -> Bool
isPrimaryKey PrimaryKey = True
isPrimaryKey _ = False

isAssignedByDatabase :: ColumnFlag -> Bool
isAssignedByDatabase AssignedByDatabase = True
isAssignedByDatabase _ = False

escapedFieldName :: FieldDefinition a -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

isPrimaryKeyField :: FieldDefinition a -> Bool
isPrimaryKeyField field = any isPrimaryKey $ fieldFlags field

withFlag :: FieldDefinition a -> ColumnFlag -> FieldDefinition a
withFlag field newFlag = field {fieldFlags = newFlag : fieldFlags field}

withName :: FieldDefinition a -> String -> FieldDefinition a
withName field newName = field {fieldName = newName}

withConversion ::
     FieldDefinition a -> (SqlType a -> SqlType b) -> FieldDefinition b
withConversion field mapType = field {fieldType = mapType $ fieldType field}

isAssignedByDatabaseField :: FieldDefinition a -> Bool
isAssignedByDatabaseField field = any isAssignedByDatabase $ fieldFlags field

withPrefix :: FieldDefinition a -> String -> FieldDefinition a
withPrefix field prefix = field `withName` (prefix ++ "_" ++ fieldName field)

fieldToNameForm :: FieldDefinition a -> NameForm
fieldToNameForm field = NameForm Nothing (fieldName field)

fieldToSqlValue :: FieldDefinition a -> a -> SqlValue
fieldToSqlValue = sqlTypeToSql . fieldType

fieldFromSqlValue :: FieldDefinition a -> SqlValue -> Maybe a
fieldFromSqlValue = sqlTypeFromSql . fieldType
