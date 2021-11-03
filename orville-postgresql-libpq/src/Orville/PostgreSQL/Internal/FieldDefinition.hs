{-# LANGUAGE GADTs #-}

{- |
Module    : Orville.PostgreSQL.Internal.FieldDefinition
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.FieldDefinition
  ( FieldDefinition,
    fieldName,
    fieldType,
    fieldIsNotNullable,
    fieldNullability,
    FieldNullability (..),
    fieldValueToSqlValue,
    fieldValueFromSqlValue,
    fieldColumnName,
    fieldColumnDefinition,
    FieldName,
    stringToFieldName,
    fieldNameToString,
    fieldNameToColumnName,
    fieldNameToByteString,
    NotNull,
    Nullable,
    convertField,
    coerceField,
    nullableField,
    asymmetricNullableField,
    integerField,
    serialField,
    smallIntegerField,
    bigIntegerField,
    bigSerialField,
    doubleField,
    booleanField,
    unboundedTextField,
    boundedTextField,
    fixedTextField,
    textSearchVectorField,
    dateField,
    utcTimestampField,
    localTimestampField,
    fieldOfType,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Coerce as Coerce
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Time as Time

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype FieldName
  = FieldName B8.ByteString
  deriving (Eq, Ord, Show)

fieldNameToColumnName :: FieldName -> Expr.ColumnName
fieldNameToColumnName (FieldName name) =
  Expr.fromIdentifier (Expr.identifierFromBytes name)

stringToFieldName :: String -> FieldName
stringToFieldName =
  FieldName . B8.pack

fieldNameToString :: FieldName -> String
fieldNameToString =
  B8.unpack . fieldNameToByteString

fieldNameToByteString :: FieldName -> B8.ByteString
fieldNameToByteString (FieldName name) =
  name

{- |
  'FieldDefinition' determines the SQL constsruction of a column in the
  database, comprising the name, SQL type and whether the field is nullable.
  A 'FieldDefinition' is matched to a particular Haskell type, which it knows
  how to marshall to and from the database representation of SQL type for
  the field.
-}
data FieldDefinition nullability a = FieldDefinition
  { _fieldName :: FieldName
  , _fieldType :: SqlType.SqlType a
  , _fieldNullability :: NullabilityGADT nullability
  }

{- |
  The name used in database queries to reference the field.
-}
fieldName :: FieldDefinition nullability a -> FieldName
fieldName = _fieldName

{- |
  The 'SqlType' for the 'FieldDefinition' determines the PostgreSQL data type
  used to define the field as well as how to mashall Haskell values to and
  from the database.
-}
fieldType :: FieldDefinition nullability a -> SqlType.SqlType a
fieldType = _fieldType

{- | A 'FieldNullability is returned by the 'fieldNullability' function, which
 can be used when a function works on both 'Nullable' and 'NotNull' functions
 but needs to deal with each type of field separately. It adds wrapper
 constructors around the 'FieldDefinition' that you can pattern match on to
 then work with a concrete 'Nullable' or 'NotNull' field.
-}
data FieldNullability a
  = NullableField (FieldDefinition Nullable a)
  | NotNullField (FieldDefinition NotNull a)

{- | Resolves the 'nullablity' of a field to a concrete type, which is returned
 via the 'FieldNullability' type. You can pattern match on this type to then
 extract the either 'Nullable' or 'NotNull' not field for cases where you
 may require different logic based on the nullability of a field.
-}
fieldNullability :: FieldDefinition nullability a -> FieldNullability a
fieldNullability field =
  case _fieldNullability field of
    NullableGADT -> NullableField field
    NotNullGADT -> NotNullField field

{- |
  Indicates whether a field is nullable.
-}
fieldIsNotNullable :: FieldDefinition nullability a -> Bool
fieldIsNotNullable field =
  case _fieldNullability field of
    NullableGADT -> False
    NotNullGADT -> True

{- |
  Mashalls a Haskell value to be stored in the field to its 'SqlValue'
  representation.
-}
fieldValueToSqlValue :: FieldDefinition nullability a -> a -> SqlValue.SqlValue
fieldValueToSqlValue =
  SqlType.sqlTypeToSql . fieldType

{- |
  Marshalls a 'SqlValue' from the database into the Haskell value that represents it.
  This may fail, in which case 'Nothing' is returned.
-}
fieldValueFromSqlValue :: FieldDefinition nullability a -> SqlValue.SqlValue -> Maybe a
fieldValueFromSqlValue =
  SqlType.sqlTypeFromSql . fieldType

{- |
  Constructs the 'Expr.ColumnName' for a field for use in SQL expressions
  from the 'Expr' module.
-}
fieldColumnName :: FieldDefinition nullability a -> Expr.ColumnName
fieldColumnName =
  fieldNameToColumnName . fieldName

{- |
  Constructions the equivalant 'Expr.FieldDefinition' as a SQL expression,
  generally for use in DDL for creating column in a table.
-}
fieldColumnDefinition :: FieldDefinition nullability a -> Expr.ColumnDefinition
fieldColumnDefinition fieldDef =
  Expr.columnDefinition
    (fieldColumnName fieldDef)
    (SqlType.sqlTypeExpr $ fieldType fieldDef)
    (Just $ fieldColumnConstraint fieldDef)

{- |
  INTERNAL - Builds the appropriate ColumnConstraint for a field. Currently
  this only handles nullability, but if we add support for more constraints
  directly on columns it may end up handling those as well.
-}
fieldColumnConstraint :: FieldDefinition nullabily a -> Expr.ColumnConstraint
fieldColumnConstraint fieldDef =
  case fieldNullability fieldDef of
    NotNullField _ ->
      Expr.notNullConstraint
    NullableField _ ->
      Expr.nullConstraint

{- |
  The type in considered internal because it requires GADTs to make use of
  it meaningfully. The 'FieldNullability' type is used as the public interface
  to surface this information to users outside the module.

  The 'NullabilityGADT' represents whether a field will be marked as 'NULL' or
  'NOT NULL' in the database schema. It is a GADT so that the value
  constructors can be used to record this knowledge in the type system as well.
  This allows functions that work only on 'Nullable' or 'NotNull' fields to
  indicate this in their type signatures as appropriate.
-}
data NullabilityGADT nullability where
  NullableGADT :: NullabilityGADT Nullable
  NotNullGADT :: NullabilityGADT NotNull

{- |
  'NotNull' is a values-less type used to track that a 'FieldDefinition'
  represents a field that is marked not-null in the database schema.  See the
  'Nullability' type for the value-level representation of field nullability.
-}
data NotNull

{- |
  'Nullable' is a values-less type used to track that a 'FieldDefinition'
  represents a field that is marked nullable in the database schema. See the
  'Nullability' type for the value-level representation of field nullability.
-}
data Nullable

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int32' values as the
  PostgreSQL "INT" type.
-}
integerField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Int32
integerField = fieldOfType SqlType.integer

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int16' values as the
  PostgreSQL "SMALLINT" type.
-}
smallIntegerField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Int16
smallIntegerField = fieldOfType SqlType.smallInteger

{- |
  Builds a 'FieldDefinition' that stores an 'Int32' value as the "SERIAL"
  type. This can be used to create auto-incrementing columns.
-}
serialField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Int32
serialField = fieldOfType SqlType.serial

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int64' values as the
  PostgreSQL "BIGINT" type.
-}
bigIntegerField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Int64
bigIntegerField = fieldOfType SqlType.bigInteger

{- |
  Builds a 'FieldDefinition' that stores an 'Int64' value as the "BIGSERIAL"
  type. This can be used to create auto-incrementing columns.
-}
bigSerialField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Int64
bigSerialField = fieldOfType SqlType.bigSerial

{- |
  Builds a 'FieldDefinition' that stores a 'Double' value as the "DOUBLE
  PRECISION" type. Note: PostgreSQL's "DOUBLE PRECISION" type only allows for
  up to 15 digits of precision, so some rounding may occur when values are
  stored in the database.
-}
doubleField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Double
doubleField = fieldOfType SqlType.double

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Bool' values as the
  PostgreSQL "BOOLEAN" type.
-}
booleanField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Bool
booleanField = fieldOfType SqlType.boolean

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "TEXT" type. Note that this PostgreSQL has no particular
  limit on the length of text stored.
-}
unboundedTextField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull T.Text
unboundedTextField = fieldOfType SqlType.unboundedText

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "VARCHAR" type. Attempting to store a value beyond the length
  specified will cause an error.

  -- TODO: We should have a test for this.
-}
boundedTextField ::
  -- | Name of the field in the database
  String ->
  -- | Maximum length of text in the field
  Int32 ->
  FieldDefinition NotNull T.Text
boundedTextField name len = fieldOfType (SqlType.boundedText len) name

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "CHAR" type. Attempting to store a value beyond the length
  specified will cause an error. Storing a value that is not the full
  length of the field will result in padding by the database.

  -- TODO: We should have a test for this.
-}
fixedTextField ::
  -- | Name of the field in the database
  String ->
  -- | Maximum length of text in the field
  Int32 ->
  FieldDefinition NotNull T.Text
fixedTextField name len = fieldOfType (SqlType.fixedText len) name

{- |
  TODO: write meaningful docs for this when we build a better Haskell
  definition for representing text search vectors.
-}
textSearchVectorField :: String -> FieldDefinition NotNull T.Text
textSearchVectorField = fieldOfType SqlType.textSearchVector

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.Day' values as the
  PostgreSQL "DATE" type.
-}
dateField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Time.Day
dateField = fieldOfType SqlType.date

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.UTCTime values as the
  PostgreSQL "TIMESTAMP with time zone" type.
-}
utcTimestampField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Time.UTCTime
utcTimestampField = fieldOfType SqlType.timestamp

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.UTCTime values as the
  PostgreSQL "TIMESTAMP without time zone" type.
-}
localTimestampField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull Time.LocalTime
localTimestampField = fieldOfType SqlType.timestampWithoutZone

{- |
  Builds a 'FieldDefinition' for will use the given 'SqlType' to determine
  the database representation of the field. If you have created a custom
  'SqlType', you can use this function to construct a helper like the
  other functions in this module for creating 'FieldDefinition's for your
  custom type.
-}
fieldOfType ::
  -- | 'SqlType' that represents the PostgreSQL data type for the field.
  SqlType.SqlType a ->
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull a
fieldOfType sqlType name =
  FieldDefinition
    (stringToFieldName name)
    sqlType
    NotNullGADT

{- |
  Makes a 'NotNull' field 'Nullable' by wrapping the Haskell type of the field
  in 'Maybe'. The field will be marked as 'NULL' in the database schema and
  the value 'Nothing' will be used to represent 'NULL' values when converting
  to and from sql.
-}
nullableField :: FieldDefinition NotNull a -> FieldDefinition Nullable (Maybe a)
nullableField field =
  let nullableType :: SqlType.SqlType a -> SqlType.SqlType (Maybe a)
      nullableType sqlType =
        sqlType
          { SqlType.sqlTypeToSql = maybe SqlValue.sqlNull (SqlType.sqlTypeToSql sqlType)
          , SqlType.sqlTypeFromSql =
              \sqlValue ->
                if SqlValue.isSqlNull sqlValue
                  then Just Nothing
                  else Just <$> SqlType.sqlTypeFromSql sqlType sqlValue
          }
   in FieldDefinition
        (fieldName field)
        (nullableType $ fieldType field)
        NullableGADT

{- |
  Adds a `Maybe` wrapper to a field that is already nullable. (If your field is
  'NotNull', you wanted 'nullableField' instead of this function). Note that
  fields created using this function have asymetric encoding and decoding of
  'NULL' values. Because the provided field is 'Nullable', 'NULL' values decode
  from the database already have a representation in the 'a' type, so 'NULL'
  will be decoded as 'Just <value of type a for NULL>'. This means if you
  insert a 'Nothing' value using the field, it will be read back as 'Just'
  value. This is useful for building high level combinators that might need to
  make fields 'Nullable' but need the value to be decoded in its underlying
  type when reading back (e.g. 'maybeMapper' from 'SqlMarshaller').
-}
asymmetricNullableField :: FieldDefinition Nullable a -> FieldDefinition Nullable (Maybe a)
asymmetricNullableField field =
  let nullableType :: SqlType.SqlType a -> SqlType.SqlType (Maybe a)
      nullableType sqlType =
        sqlType
          { SqlType.sqlTypeToSql = maybe SqlValue.sqlNull (SqlType.sqlTypeToSql sqlType)
          , SqlType.sqlTypeFromSql = \sqlValue -> Just <$> SqlType.sqlTypeFromSql sqlType sqlValue
          }
   in FieldDefinition
        (fieldName field)
        (nullableType $ fieldType field)
        NullableGADT

{- |
  Applies a 'SqlType.SqlType' conversion to a 'FieldDefinition'. You can
  use this function the create 'FieldDefinition's for based on the primitive
  ones provided, but with more specific Haskell types.

  See 'SqlType.convertSqlType' and 'SqlType.maybeConvertSqlType' for functions
  to create the conversion needed as the firts argument to 'convertField'.
-}
convertField ::
  (SqlType.SqlType a -> SqlType.SqlType b) ->
  FieldDefinition nullability a ->
  FieldDefinition nullability b
convertField conversion fieldDef =
  fieldDef
    { _fieldType = conversion (_fieldType fieldDef)
    }

{- |
  A specicialization of 'convertField' that can be used with types that
  implement 'Coere.Coercible'. This is particularly useful for newtype wrappers
  around primitive types.
-}
coerceField ::
  (Coerce.Coercible a b, Coerce.Coercible b a) =>
  FieldDefinition nullability a ->
  FieldDefinition nullability b
coerceField =
  convertField
    (SqlType.convertSqlType Coerce.coerce Coerce.coerce)
