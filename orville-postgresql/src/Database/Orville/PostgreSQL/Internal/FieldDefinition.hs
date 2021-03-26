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

textField :: String -> Int -> FieldDefinition NotNull Text
textField name len = fieldOfType (varText len) name

fixedTextField :: String -> Int -> FieldDefinition NotNull Text
fixedTextField name len = fieldOfType (text len) name

unboundedTextField :: String -> FieldDefinition NotNull Text
unboundedTextField = fieldOfType unboundedText

dayField :: String -> FieldDefinition NotNull Day
dayField = fieldOfType date

utcTimeField :: String -> FieldDefinition NotNull UTCTime
utcTimeField = fieldOfType timestamp

int32Field :: String -> FieldDefinition NotNull Int32
int32Field = fieldOfType integer

int64Field :: String -> FieldDefinition NotNull Int64
int64Field = fieldOfType bigInteger

doubleField :: String -> FieldDefinition NotNull Double
doubleField = fieldOfType double

boolField :: String -> FieldDefinition NotNull Bool
boolField = fieldOfType boolean

automaticIdField :: String -> FieldDefinition NotNull Int32
automaticIdField = fieldOfType serial

searchVectorField :: String -> FieldDefinition NotNull Text
searchVectorField = fieldOfType textSearchVector

{-|
  Makes a 'NotNull' field 'Nullable' by wrapping the Haskell type of the field
  in 'Maybe'. The field will be marked as 'NULL' in the database schema and
  the value 'Nothing' will be used to represent 'NULL' values when converting
  to and from sql.
-}
nullableField :: FieldDefinition NotNull a -> FieldDefinition Nullable (Maybe a)
nullableField field =
  let
    nullableType sqlType =
      sqlType
        { sqlTypeToSql = maybe SqlNull (sqlTypeToSql sqlType)
        , sqlTypeFromSql =
            \sql ->
              case sql of
                SqlNull ->
                  Just Nothing

                _ ->
                  Just <$> sqlTypeFromSql sqlType sql
        }
  in
    FieldDefinition
      (fieldName field)
      (nullableType $ fieldType field)
      (fieldFlags field)
      Nullable

{-|
  Adds a `Maybe` wrapper to a field that is already nullable. (If your field is
  'NotNull', you wanted 'nullableField' instead of this function). Note that
  fields created using this function have asymetric encoding and decoding of
  'NULL' values. Because the provided field is 'Nullable', 'NULL' values decode
  from the database already have a representation in the 'a' type, so 'NULL'
  will be decoded as 'Just <value of type a for NULL>'. This means if you
  insert a 'Nothing' value using the field, it will be read back as 'Just'
  value. This is useful for building high level combinators that might need to
  make fields 'Nullable' but need the value to be decoded in its underlying
  type when reading back (e.g. 'maybeMapper' from 'RelationalMap').
-}
asymmetricNullableField :: FieldDefinition Nullable a -> FieldDefinition Nullable (Maybe a)
asymmetricNullableField field =
  let
    nullableType sqlType =
      sqlType
        { sqlTypeToSql = maybe SqlNull (sqlTypeToSql sqlType)
        , sqlTypeFromSql = \sql -> Just <$> sqlTypeFromSql sqlType sql
        }
  in
    FieldDefinition
      (fieldName field)
      (nullableType $ fieldType field)
      (fieldFlags field)
      Nullable

isFieldNullable :: FieldDefinition nullability a -> Bool
isFieldNullable field =
  case checkNullability field of
    NullableField _ -> True
    NotNullField _ -> False

foreignKeyField ::
     String
  -> TableDefinition readEntity writeEntity key
  -> FieldDefinition nullability key
  -> FieldDefinition nullability key
foreignKeyField name refTable refField =
  FieldDefinition
    name
    (foreignRefType $ fieldType refField)
    [References refTable refField]
    (fieldNullability refField)

fieldOfType :: SqlType a -> String -> FieldDefinition NotNull a
fieldOfType sqlType name =
  FieldDefinition
    name
    sqlType
    []
    NotNull

isPrimaryKey :: ColumnFlag -> Bool
isPrimaryKey PrimaryKey = True
isPrimaryKey _ = False

isAssignedByDatabase :: ColumnFlag -> Bool
isAssignedByDatabase AssignedByDatabase = True
isAssignedByDatabase _ = False

escapedFieldName :: FieldDefinition nullability a -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

withFlag :: FieldDefinition nullability a -> ColumnFlag -> FieldDefinition nullability a
withFlag field newFlag = field {fieldFlags = newFlag : fieldFlags field}

withName :: FieldDefinition nullability a -> String -> FieldDefinition nullability a
withName field newName = field {fieldName = newName}

withConversion ::
     FieldDefinition nullability a -> (SqlType a -> SqlType b) -> FieldDefinition nullability b
withConversion field mapType = field {fieldType = mapType $ fieldType field}

isAssignedByDatabaseField :: FieldDefinition nullability a -> Bool
isAssignedByDatabaseField field = any isAssignedByDatabase $ fieldFlags field

withPrefix :: FieldDefinition nullability a -> String -> FieldDefinition nullability a
withPrefix field prefix = field `withName` (prefix ++ "_" ++ fieldName field)

fieldToNameForm :: FieldDefinition nullability a -> NameForm
fieldToNameForm field = NameForm Nothing (fieldName field)

fieldToSqlValue :: FieldDefinition nullability a -> a -> SqlValue
fieldToSqlValue = sqlTypeToSql . fieldType

fieldFromSqlValue :: FieldDefinition nullability a -> SqlValue -> Maybe a
fieldFromSqlValue = sqlTypeFromSql . fieldType
