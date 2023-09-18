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

{- |
  Migration Guide: @textField@ has been renamed to @boundedTextField@. It now
  takes an @Int32@ rather than an @Int@
-}
textField :: String -> Int -> FieldDefinition NotNull Text
textField name len = fieldOfType (varText len) name

{- |
  Migration Guide: @fixedTextField@ retains the same name. It now
  takes an @Int32@ rather than an @Int@
-}
fixedTextField :: String -> Int -> FieldDefinition NotNull Text
fixedTextField name len = fieldOfType (text len) name

{- |
  Migration Guide: @unboundedTextField@ retains the same name.
-}
unboundedTextField :: String -> FieldDefinition NotNull Text
unboundedTextField = fieldOfType unboundedText

{- |
  Migration Guide: @dayField@ has been renamed to @dateField@
-}
dayField :: String -> FieldDefinition NotNull Day
dayField = fieldOfType date

{- |
  Migration Guide: @utcTimeField@ has been renamed to @utcTimestampField@
-}
utcTimeField :: String -> FieldDefinition NotNull UTCTime
utcTimeField = fieldOfType timestamp

{- |
  Migration guide: @int32Field@ has been renamed to @integerField@
-}
int32Field :: String -> FieldDefinition NotNull Int32
int32Field = fieldOfType integer

{- |
  Migration guide: @int64Field@ has been renamed to @bigIntegerField@
-}
int64Field :: String -> FieldDefinition NotNull Int64
int64Field = fieldOfType bigInteger

{- |
  Migration guide: @doubleField@ retains the same name.
-}
doubleField :: String -> FieldDefinition NotNull Double
doubleField = fieldOfType double

{- |
  Migration guide: @boolField@ has been renamed to @booleanField@
-}
boolField :: String -> FieldDefinition NotNull Bool
boolField = fieldOfType boolean

{- |
  Migration guide: @automaticIdField@ has been renamed to @serialField@
-}
automaticIdField :: String -> FieldDefinition NotNull Int32
automaticIdField = fieldOfType serial

{- |
  Migration guide: @searchVectorField@ has been renamed to @textSearchVectorField@
-}
searchVectorField :: String -> FieldDefinition NotNull Text
searchVectorField = fieldOfType textSearchVector

{-|
  Migration Guide: @nullableField@ retains the same name

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
                  Right Nothing

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

{- |
  Migration Guide: @isFieldNullable@ has been replaced with
  @fieldIsNotNullable@, which has the same signture but the @Bool@ returned is
  the opposite.
-}
isFieldNullable :: FieldDefinition nullability a -> Bool
isFieldNullable field =
  case checkNullability field of
    NullableField _ -> True
    NotNullField _ -> False

{- |
  Migration Guide: @foreignKeyField@ has been removed. It is replaced by
  @addForeignKeyConstraint@ which adds a foreign key constraint to an existing
  @FieldDefinition@.
-}
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

{- |
  Migration Guide: @fieldOfType@ is essentially unchanged in the new orville.
-}
fieldOfType :: SqlType a -> String -> FieldDefinition NotNull a
fieldOfType sqlType name =
  FieldDefinition
    name
    sqlType
    []
    NotNull

isAssignedByDatabase :: ColumnFlag -> Bool
isAssignedByDatabase AssignedByDatabase = True
isAssignedByDatabase _ = False

escapedFieldName :: FieldDefinition nullability a -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

{- |
  Migration Guide: @withFlag@ has been removed. See the migration guide
  on 'ColumnFlag' regarding the new API.
-}
withFlag :: FieldDefinition nullability a -> ColumnFlag -> FieldDefinition nullability a
withFlag field newFlag = field {fieldFlags = newFlag : fieldFlags field}

{- |
  Migration Guide: @withName@ has been removed.
-}
withName :: FieldDefinition nullability a -> String -> FieldDefinition nullability a
withName field newName = field {fieldName = newName}

{- |
  Migration Guide: @withConversion@ has been replaced with @convertField@,
  whose arguments are flipped from those of @withConversion@. Note there is
  also now a @coerceField@ function that can be used with @newtype@ wrappers,
  provided the constructor is available where @coerceField@ is used.
-}
withConversion ::
     FieldDefinition nullability a -> (SqlType a -> SqlType b) -> FieldDefinition nullability b
withConversion field mapType = field {fieldType = mapType $ fieldType field}

isAssignedByDatabaseField :: FieldDefinition nullability a -> Bool
isAssignedByDatabaseField field = any isAssignedByDatabase $ fieldFlags field

{- |
  Migration Guide: @withPrefix@ has been replaced by @prefixField@ whose
  arguments are flipped relative to @withPrefix@
-}
withPrefix :: FieldDefinition nullability a -> String -> FieldDefinition nullability a
withPrefix field prefix = field `withName` (prefix ++ "_" ++ fieldName field)

fieldToNameForm :: FieldDefinition nullability a -> NameForm
fieldToNameForm field = NameForm Nothing (fieldName field)

{- |
  Migration Guide: @fieldToSqlValue@ has been renamed to @fieldValueToSqlValue@
-}
fieldToSqlValue :: FieldDefinition nullability a -> a -> SqlValue
fieldToSqlValue = sqlTypeToSql . fieldType

{- |
  Migration Guide: @fieldFromSqlValue@ has been renamed to @fieldValueFromSqlValue@
-}
fieldFromSqlValue :: FieldDefinition nullability a -> SqlValue -> Either RowDataErrorReason a
fieldFromSqlValue = sqlTypeFromSql . fieldType
