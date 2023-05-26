{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Marshall.FieldDefinition
  ( FieldDefinition,
    fieldName,
    fieldDescription,
    setFieldDescription,
    fieldType,
    fieldIsNotNullable,
    fieldDefaultValue,
    fieldNullability,
    fieldEquals,
    (.==),
    fieldNotEquals,
    (./=),
    fieldGreaterThan,
    (.>),
    fieldLessThan,
    (.<),
    fieldGreaterThanOrEqualTo,
    (.>=),
    fieldLessThanOrEqualTo,
    (.<=),
    fieldIsNull,
    fieldIsNotNull,
    fieldLike,
    fieldLikeInsensitive,
    fieldIn,
    (.<-),
    fieldNotIn,
    (.</-),
    fieldTupleIn,
    fieldTupleNotIn,
    setField,
    (.:=),
    orderByField,
    FieldNullability (..),
    fieldValueToExpression,
    fieldValueToSqlValue,
    fieldValueFromSqlValue,
    fieldColumnName,
    fieldColumnReference,
    fieldColumnDefinition,
    FieldName,
    stringToFieldName,
    fieldNameToString,
    fieldNameToColumnName,
    fieldNameToByteString,
    byteStringToFieldName,
    NotNull,
    Nullable,
    convertField,
    coerceField,
    nullableField,
    asymmetricNullableField,
    setDefaultValue,
    removeDefaultValue,
    prefixField,
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
    uuidField,
    fieldOfType,
    whereColumnComparison,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Coerce as Coerce
import Data.Int (Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.UUID as UUID

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.DefaultValue as DefaultValue
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

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

byteStringToFieldName :: B8.ByteString -> FieldName
byteStringToFieldName = FieldName

{- |
  'FieldDefinition' determines the SQL constsruction of a column in the
  database, comprising the name, SQL type and whether the field is nullable.
  A 'FieldDefinition' is matched to a particular Haskell type, which it knows
  how to marshall to and from the database representation of SQL type for
  the field.
-}
data FieldDefinition nullability a = FieldDefinition
  { i_fieldName :: FieldName
  , i_fieldType :: SqlType.SqlType a
  , i_fieldNullability :: NullabilityGADT nullability
  , i_fieldDefaultValue :: Maybe (DefaultValue.DefaultValue a)
  , i_fieldDescription :: Maybe String
  }

{- |
  The name used in database queries to reference the field.
-}
fieldName :: FieldDefinition nullability a -> FieldName
fieldName = i_fieldName

{- |
  Returns the description that was passed to 'setFieldDescription', if any.
-}
fieldDescription :: FieldDefinition nullability a -> Maybe String
fieldDescription = i_fieldDescription

{- |
  Sets the description for the field. This description not currently used
  anywhere by Orville itself, but users can retrieve the description via
  'fieldDescription' for their own purposes (e.g. generating documentation).
-}
setFieldDescription :: String -> FieldDefinition nullability a -> FieldDefinition nullability a
setFieldDescription description fieldDef =
  fieldDef
    { i_fieldDescription = Just description
    }

{- |
  The 'SqlType' for the 'FieldDefinition' determines the PostgreSQL data type
  used to define the field as well as how to mashall Haskell values to and
  from the database.
-}
fieldType :: FieldDefinition nullability a -> SqlType.SqlType a
fieldType = i_fieldType

{- |
  Returns the default value definition for the field, if any has been set.
-}
fieldDefaultValue :: FieldDefinition nullability a -> Maybe (DefaultValue.DefaultValue a)
fieldDefaultValue = i_fieldDefaultValue

{- |
 A 'FieldNullability' is returned by the 'fieldNullability' function, which
 can be used when a function works on both 'Nullable' and 'NotNull' functions
 but needs to deal with each type of field separately. It adds wrapper
 constructors around the 'FieldDefinition' that you can pattern match on to
 then work with a concrete 'Nullable' or 'NotNull' field.
-}
data FieldNullability a
  = NullableField (FieldDefinition Nullable a)
  | NotNullField (FieldDefinition NotNull a)

{- |
 Resolves the 'nullablity' of a field to a concrete type, which is returned
 via the 'FieldNullability' type. You can pattern match on this type to then
 extract the either 'Nullable' or 'NotNull' not field for cases where you
 may require different logic based on the nullability of a field.
-}
fieldNullability :: FieldDefinition nullability a -> FieldNullability a
fieldNullability field =
  case i_fieldNullability field of
    NullableGADT -> NullableField field
    NotNullGADT -> NotNullField field

{- |
  Indicates whether a field is nullable.
-}
fieldIsNotNullable :: FieldDefinition nullability a -> Bool
fieldIsNotNullable field =
  case i_fieldNullability field of
    NullableGADT -> False
    NotNullGADT -> True

{- |
  Mashalls a Haskell value to be stored in the field to its 'SqlValue'
  representation and packages the resul as a 'Expr.ValueExression' so that
  it can be easily used with other @Expr@ functions.
-}
fieldValueToExpression :: FieldDefinition nullability a -> a -> Expr.ValueExpression
fieldValueToExpression field =
  Expr.valueExpression . fieldValueToSqlValue field

{- |
  Mashalls a Haskell value to be stored in the field to its 'SqlValue'
  representation.
-}
fieldValueToSqlValue :: FieldDefinition nullability a -> a -> SqlValue.SqlValue
fieldValueToSqlValue =
  SqlType.sqlTypeToSql . fieldType

{- |
  Marshalls a 'SqlValue' from the database into the Haskell value that represents it.
  This may fail, in which case a 'Left' is returned with an error message.
-}
fieldValueFromSqlValue :: FieldDefinition nullability a -> SqlValue.SqlValue -> Either String a
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
  Constructs the 'Expr.ValueExpression for a field for use in SQL expressions
  from the 'Expr' module.
-}
fieldColumnReference :: FieldDefinition nullability a -> Expr.ValueExpression
fieldColumnReference =
  Expr.columnReference . fieldColumnName

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
    (fmap (Expr.columnDefault . DefaultValue.defaultValueExpression) $ i_fieldDefaultValue fieldDef)

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
  Builds a 'FieldDefinition' that stores Haskell 'UUID.UUID' values as the
  PostgreSQL "UUID" type.
-}
uuidField ::
  -- | Name of the field in the database
  String ->
  FieldDefinition NotNull UUID.UUID
uuidField = fieldOfType SqlType.uuid

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
    { i_fieldName = stringToFieldName name
    , i_fieldType = sqlType
    , i_fieldNullability = NotNullGADT
    , i_fieldDefaultValue = Nothing
    , i_fieldDescription = Nothing
    }

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
                  then Right Nothing
                  else Just <$> SqlType.sqlTypeFromSql sqlType sqlValue
          }
   in FieldDefinition
        { i_fieldName = fieldName field
        , i_fieldType = nullableType (fieldType field)
        , i_fieldNullability = NullableGADT
        , i_fieldDefaultValue = fmap DefaultValue.coerceDefaultValue (i_fieldDefaultValue field)
        , i_fieldDescription = fieldDescription field
        }

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
        { i_fieldName = fieldName field
        , i_fieldType = nullableType (fieldType field)
        , i_fieldNullability = NullableGADT
        , i_fieldDefaultValue = fmap DefaultValue.coerceDefaultValue (i_fieldDefaultValue field)
        , i_fieldDescription = fieldDescription field
        }

{- |
  Applies a 'SqlType.SqlType' conversion to a 'FieldDefinition'. You can
  use this function the create 'FieldDefinition's for based on the primitive
  ones provided, but with more specific Haskell types.

  See 'SqlType.convertSqlType' and 'SqlType.tryConvertSqlType' for functions
  to create the conversion needed as the first argument to 'convertField'.
-}
convertField ::
  (SqlType.SqlType a -> SqlType.SqlType b) ->
  FieldDefinition nullability a ->
  FieldDefinition nullability b
convertField conversion fieldDef =
  fieldDef
    { i_fieldType = conversion (i_fieldType fieldDef)
    , i_fieldDefaultValue = fmap DefaultValue.coerceDefaultValue (i_fieldDefaultValue fieldDef)
    }

{- |
  A specialization of 'convertField' that can be used with types that implement
  'Coere.Coercible'. This is particularly useful for newtype wrappers around
  primitive types.
-}
coerceField ::
  (Coerce.Coercible a b, Coerce.Coercible b a) =>
  FieldDefinition nullability a ->
  FieldDefinition nullability b
coerceField =
  convertField
    (SqlType.convertSqlType Coerce.coerce Coerce.coerce)

{- |
  Sets a default value for the field. The default value will be added as part
  of the column definition in the database. Because the default value is
  ultimately provided by the database this can be used to add a not-null column
  to safely to an existing table as long as a reasonable default value is
  available to use.
-}
setDefaultValue ::
  DefaultValue.DefaultValue a ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
setDefaultValue defaultValue fieldDef =
  fieldDef
    { i_fieldDefaultValue = Just defaultValue
    }

{- |
  Removes any default value that may have been set on a field via
  @setDefaultValue@.
-}
removeDefaultValue ::
  FieldDefinition nullability a ->
  FieldDefinition nullability a
removeDefaultValue fieldDef =
  fieldDef
    { i_fieldDefaultValue = Nothing
    }

{- |
  Adds a prefix, followed by an underscore, to a field's name.
-}
prefixField ::
  String ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
prefixField prefix fieldDef =
  fieldDef
    { i_fieldName = FieldName (B8.pack prefix <> "_" <> fieldNameToByteString (fieldName fieldDef))
    }

{- |
  Constructs a 'Expr.SetClause' that will set the column named in the
  field definition to the given value. The value is be converted to SQL
  value using 'fieldValueToSqlValue'
-}
setField :: FieldDefinition nullability a -> a -> Expr.SetClause
setField fieldDef value =
  Expr.setColumn
    (fieldColumnName fieldDef)
    (fieldValueToSqlValue fieldDef value)

{- |
  Operator alias for 'setField'
-}
(.:=) :: FieldDefinition nullability a -> a -> Expr.SetClause
(.:=) = setField

{- |
  Checks that the value in a field equals a particular value.
-}
fieldEquals :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldEquals =
  whereColumnComparison Expr.equals

{- |
  Operator alias for 'fieldEquals'
-}
(.==) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.==) = fieldEquals

infixl 9 .==

{- |
  Checks that the value in a field does not equal a particular value.
-}
fieldNotEquals :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldNotEquals =
  whereColumnComparison Expr.notEquals

{- |
  Operator alias for 'fieldNotEquals'
-}
(./=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(./=) = fieldNotEquals

infixl 9 ./=

{- |
  Checks that the value in a field is greater than a particular value.
-}
fieldGreaterThan :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldGreaterThan =
  whereColumnComparison Expr.greaterThan

{- |
  Operator alias for 'fieldGreaterThan'
-}
(.>) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.>) = fieldGreaterThan

infixl 9 .>

{- |
  Checks that the value in a field is less than a particular value.
-}
fieldLessThan :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldLessThan =
  whereColumnComparison Expr.lessThan

{- |
  Operator alias for 'fieldLessThan'
-}
(.<) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.<) = fieldLessThan

infixl 9 .<

{- |
  Checks that the value in a field is greater than or equal to a particular value.
-}
fieldGreaterThanOrEqualTo :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldGreaterThanOrEqualTo =
  whereColumnComparison Expr.greaterThanOrEqualTo

{- |
  Operator alias for 'fieldGreaterThanOrEqualTo'
-}
(.>=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.>=) = fieldGreaterThanOrEqualTo

infixl 9 .>=

{- |
  Checks that the value in a field is less than or equal to a particular value.
-}
fieldLessThanOrEqualTo :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldLessThanOrEqualTo =
  whereColumnComparison Expr.lessThanOrEqualTo

{- |
  Operator alias for 'fieldLessThanOrEqualTo'
-}
(.<=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.<=) = fieldLessThanOrEqualTo

infixl 9 .<=

{- |
  Checks that the value in a field matches a like pattern
-}
fieldLike :: FieldDefinition nullability a -> T.Text -> Expr.BooleanExpr
fieldLike fieldDef likePattern =
  Expr.like
    (fieldColumnReference fieldDef)
    (Expr.valueExpression (SqlValue.fromText likePattern))

{- |
  Checks that the value in a field matches a like pattern case insensitively
-}
fieldLikeInsensitive :: FieldDefinition nullability a -> T.Text -> Expr.BooleanExpr
fieldLikeInsensitive fieldDef likePattern =
  Expr.likeInsensitive
    (fieldColumnReference fieldDef)
    (Expr.valueExpression (SqlValue.fromText likePattern))

{- |
  Checks that the value in a field is null.
-}
fieldIsNull :: FieldDefinition Nullable a -> Expr.BooleanExpr
fieldIsNull =
  Expr.isNull . fieldColumnReference

{- |
  Checks that the value in a field is not null.
-}
fieldIsNotNull :: FieldDefinition Nullable a -> Expr.BooleanExpr
fieldIsNotNull =
  Expr.isNotNull . fieldColumnReference

{- |
  Checks that a field matches a list of values
-}
fieldIn :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
fieldIn fieldDef values =
  Expr.valueIn
    (fieldColumnReference fieldDef)
    (fmap (fieldValueToExpression fieldDef) values)

{- |
  Operator alias for 'fieldIn'
-}
(.<-) :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
(.<-) = fieldIn

infixl 9 .<-

{- |
  Checks that a field does not match a list of values
-}
fieldNotIn :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
fieldNotIn fieldDef values =
  Expr.valueNotIn
    (fieldColumnReference fieldDef)
    (fmap (fieldValueToExpression fieldDef) values)

{- |
  Operator alias for 'fieldNotIn'
-}
(.</-) :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
(.</-) = fieldNotIn

infixl 9 .</-

{- |
  Checks that a tuple of two fields is in the list of specified tuplies
-}
fieldTupleIn ::
  FieldDefinition nullabilityA a ->
  FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  Expr.BooleanExpr
fieldTupleIn fieldDefA fieldDefB values =
  Expr.tupleIn
    (fieldColumnReference fieldDefA :| [fieldColumnReference fieldDefB])
    (fmap (toSqlValueTuple fieldDefA fieldDefB) values)

{- |
  Checks that a tuple of two fields is not in the list of specified tuplies
-}
fieldTupleNotIn ::
  FieldDefinition nullabilityA a ->
  FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  Expr.BooleanExpr
fieldTupleNotIn fieldDefA fieldDefB values =
  Expr.tupleNotIn
    (fieldColumnReference fieldDefA :| [fieldColumnReference fieldDefB])
    (fmap (toSqlValueTuple fieldDefA fieldDefB) values)

{- |
  Constructs a SqlValue "tuple" (i.e. NonEmpty list) for two fields
-}
toSqlValueTuple ::
  FieldDefinition nullabilityA a ->
  FieldDefinition nullabilityB b ->
  (a, b) ->
  NonEmpty Expr.ValueExpression
toSqlValueTuple fieldDefA fieldDefB (a, b) =
  fieldValueToExpression fieldDefA a
    :| [fieldValueToExpression fieldDefB b]

{- |
  Constructs a field-based 'Expr.BooleanExpr' using a function that
  builds a 'Expr.BooleanExpr'
-}
whereColumnComparison ::
  (Expr.ValueExpression -> Expr.ValueExpression -> Expr.BooleanExpr) ->
  (FieldDefinition nullability a -> a -> Expr.BooleanExpr)
whereColumnComparison columnComparison fieldDef a =
  columnComparison
    (fieldColumnReference fieldDef)
    (fieldValueToExpression fieldDef a)

{-- |
  Orders a query by the column name for the given field.
--}
orderByField ::
  FieldDefinition nullability value ->
  Expr.OrderByDirection ->
  Expr.OrderByExpr
orderByField =
  Expr.orderByColumnName . fieldColumnName
