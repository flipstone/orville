{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

This module provides functions for working with Orville 'FieldDefinition'
values. 'FieldDefinition' is use to determine the column name and data type
that a Haskell field is mapped to via a
'Orville.PostgreSQL.Marhall.SqlMarshaller'. It is also used for constructing
boolean conditions for matching rows in queries.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Marshall.FieldDefinition
  ( FieldDefinition
  , fieldName
  , setFieldName
  , fieldDescription
  , setFieldDescription
  , fieldType
  , fieldIsNotNullable
  , fieldDefaultValue
  , fieldNullability
  , fieldTableConstraints
  , addFieldTableConstraints
  , addForeignKeyConstraint
  , addForeignKeyConstraintWithOptions
  , addUniqueConstraint
  , fieldEquals
  , (.==)
  , fieldNotEquals
  , (./=)
  , fieldIsDistinctFrom
  , fieldIsNotDistinctFrom
  , fieldGreaterThan
  , (.>)
  , fieldLessThan
  , (.<)
  , fieldGreaterThanOrEqualTo
  , (.>=)
  , fieldLessThanOrEqualTo
  , (.<=)
  , fieldIsNull
  , fieldIsNotNull
  , fieldLike
  , fieldLikeInsensitive
  , fieldIn
  , (.<-)
  , fieldNotIn
  , (.</-)
  , fieldTupleIn
  , fieldTupleNotIn
  , setField
  , (.:=)
  , orderByField
  , orderByAliasedField
  , FieldNullability (..)
  , fieldValueToExpression
  , fieldValueToSqlValue
  , fieldValueFromSqlValue
  , fieldColumnName
  , fieldColumnReference
  , fieldColumnDefinition
  , FieldName
  , stringToFieldName
  , fieldNameToString
  , fieldNameToColumnName
  , fieldNameToByteString
  , byteStringToFieldName
  , NotNull
  , Nullable
  , convertField
  , coerceField
  , nullableField
  , asymmetricNullableField
  , setDefaultValue
  , removeDefaultValue
  , prefixField
  , integerField
  , serialField
  , smallIntegerField
  , bigIntegerField
  , bigSerialField
  , doubleField
  , booleanField
  , unboundedTextField
  , boundedTextField
  , fixedTextField
  , textSearchVectorField
  , dateField
  , utcTimestampField
  , localTimestampField
  , uuidField
  , jsonbField
  , fieldOfType
  , whereColumnComparison
  , AliasedFieldDefinition
  , getFieldDefinition
  , getAlias
  , buildAliasedFieldDefinition
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
import Orville.PostgreSQL.Internal.FieldName (FieldName, byteStringToFieldName, fieldNameToByteString, fieldNameToColumnName, fieldNameToString, stringToFieldName)
import Orville.PostgreSQL.Marshall.AliasName (AliasName, aliasNameToAliasExpr)
import qualified Orville.PostgreSQL.Marshall.DefaultValue as DefaultValue
import Orville.PostgreSQL.Marshall.SqlComparable (SqlComparable (referenceValueExpression, toComparableSqlValue))
import qualified Orville.PostgreSQL.Marshall.SqlComparable as SqlComparable
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Schema.ConstraintDefinition as ConstraintDefinition
import qualified Orville.PostgreSQL.Schema.TableIdentifier as TableIdentifier

{- |
  'FieldDefinition' determines the SQL construction of a column in the
  database, comprising the name, SQL type and whether the field is nullable.
  A 'FieldDefinition' is matched to a particular Haskell type, which it knows
  how to marshall to and from the database representation of SQL type for
  the field.

@since 1.0.0.0
-}
data FieldDefinition nullability a = FieldDefinition
  { i_fieldName :: FieldName
  , i_fieldType :: SqlType.SqlType a
  , i_fieldNullability :: NullabilityGADT nullability
  , i_fieldDefaultValue :: Maybe (DefaultValue.DefaultValue a)
  , i_fieldDescription :: Maybe String
  , i_fieldTableConstraints :: [FieldName -> ConstraintDefinition.ConstraintDefinition]
  }

{- | Constructs the 'Expr.ValueExpression' for a field for use in SQL expressions
  from the "Orville.PostgreSQL.Expr" module.

@since 1.1.0.0
-}
instance SqlComparable.SqlComparable (FieldDefinition nullability a) a where
  toComparableSqlValue = fieldValueToSqlValue
  referenceValueExpression = fieldColumnReference

{- |
  The name used in database queries to reference the field.

@since 1.0.0.0
-}
fieldName :: FieldDefinition nullability a -> FieldName
fieldName = i_fieldName

{- |
  Sets the name used in database queries to reference the field.

@since 1.0.0.0
-}
setFieldName :: FieldName -> FieldDefinition nullability a -> FieldDefinition nullability a
setFieldName newName fieldDef =
  fieldDef
    { i_fieldName = newName
    }

{- |
  Returns the description that was passed to 'setFieldDescription', if any.

@since 1.0.0.0
-}
fieldDescription :: FieldDefinition nullability a -> Maybe String
fieldDescription = i_fieldDescription

{- |
  Sets the description for the field. This description is not currently used
  anywhere by Orville itself, but users can retrieve the description via
  'fieldDescription' for their own purposes (e.g. generating documentation).

@since 1.0.0.0
-}
setFieldDescription :: String -> FieldDefinition nullability a -> FieldDefinition nullability a
setFieldDescription description fieldDef =
  fieldDef
    { i_fieldDescription = Just description
    }

{- |
  The 'SqlType.SqlType' for the 'FieldDefinition' determines the PostgreSQL
  data type used to define the field as well as how to marshall Haskell values
  to and from the database.

@since 1.0.0.0
-}
fieldType :: FieldDefinition nullability a -> SqlType.SqlType a
fieldType = i_fieldType

{- |
  Returns the default value definition for the field, if any has been set.

@since 1.0.0.0
-}
fieldDefaultValue :: FieldDefinition nullability a -> Maybe (DefaultValue.DefaultValue a)
fieldDefaultValue = i_fieldDefaultValue

{- |
 A 'FieldNullability' is returned by the 'fieldNullability' function, which
 can be used when a function works on both 'Nullable' and 'NotNull' functions
 but needs to deal with each type of field separately. It adds wrapper
 constructors around the 'FieldDefinition' that you can pattern match on to
 then work with a concrete 'Nullable' or 'NotNull' field.

@since 1.0.0.0
-}
data FieldNullability a
  = NullableField (FieldDefinition Nullable a)
  | NotNullField (FieldDefinition NotNull a)

{- |
 Resolves the @nullability@ of a field to a concrete type, which is returned
 via the 'FieldNullability' type. You can pattern match on this type to then
 extract the either 'Nullable' or 'NotNull' field for cases where you may
 require different logic based on the nullability of a field.

@since 1.0.0.0
-}
fieldNullability :: FieldDefinition nullability a -> FieldNullability a
fieldNullability field =
  case i_fieldNullability field of
    NullableGADT -> NullableField field
    NotNullGADT -> NotNullField field

{- |
  Indicates whether a field is not nullable.

@since 1.0.0.0
-}
fieldIsNotNullable :: FieldDefinition nullability a -> Bool
fieldIsNotNullable field =
  case i_fieldNullability field of
    NullableGADT -> False
    NotNullGADT -> True

{- |
  A list of table constraints that will be included on any table that uses this
  field definition.

@since 1.0.0.0
-}
fieldTableConstraints ::
  FieldDefinition nullability a ->
  ConstraintDefinition.TableConstraints
fieldTableConstraints fieldDef =
  let
    name =
      fieldName fieldDef

    constructedConstraints =
      fmap ($ name) (i_fieldTableConstraints fieldDef)
  in
    foldr
      ConstraintDefinition.addConstraint
      ConstraintDefinition.emptyTableConstraints
      constructedConstraints

{- |
  Adds the given table constraints to the field definition. These constraints
  will then be included on any table where the field is used. The constraints
  are passed a function that will take the name of the field definition and
  construct the constraints. This allows the
  'ConstraintDefinition.ConstraintDefinition's to use the correct name of the
  field in the case where 'setFieldName' is used after constraints are added.

  Note: If multiple constraints are added with the same
  'Orville.PostgreSQL.ConstraintMigrationKey', only the last one that is added
  will be part of the 'Orville.PostgreSQL.TableDefinition'. Any previously
  added constraint with the same key is replaced by the new one.

@since 1.0.0.0
-}
addFieldTableConstraints ::
  [FieldName -> ConstraintDefinition.ConstraintDefinition] ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
addFieldTableConstraints constraintDefs fieldDef =
  fieldDef
    { i_fieldTableConstraints =
        constraintDefs <> i_fieldTableConstraints fieldDef
    }

{- |
  Adds a @FOREIGN KEY@ constraint to the 'FieldDefinition' (using
  'addFieldTableConstraints'). This constraint will be included on any table
  that uses the field definition.

@since 1.0.0.0
-}
addForeignKeyConstraint ::
  -- | Identifier of the table referenced by the foreign key.
  TableIdentifier.TableIdentifier ->
  -- | The field name that this field definition references in the foreign table.
  FieldName ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
addForeignKeyConstraint foreignTableId foreignFieldName =
  addForeignKeyConstraintWithOptions
    foreignTableId
    foreignFieldName
    ConstraintDefinition.defaultForeignKeyOptions

{- |
  Adds a @FOREIGN KEY@ constraint to the 'FieldDefinition'. This constraint
  will be included on any table that uses the field definition.

@since 1.0.0.0
-}
addForeignKeyConstraintWithOptions ::
  -- | Identifier of the table referenced by the foreign key.
  TableIdentifier.TableIdentifier ->
  -- | The field name that this field definition references in the foreign table.
  FieldName ->
  ConstraintDefinition.ForeignKeyOptions ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
addForeignKeyConstraintWithOptions foreignTableId foreignFieldName options fieldDef =
  let
    mkReference name =
      ConstraintDefinition.ForeignReference
        { ConstraintDefinition.localFieldName = name
        , ConstraintDefinition.foreignFieldName = foreignFieldName
        }

    constraintToAdd name =
      ConstraintDefinition.foreignKeyConstraintWithOptions
        foreignTableId
        (mkReference name :| [])
        options
  in
    addFieldTableConstraints [constraintToAdd] fieldDef

{- |
  Adds a @UNIQUE@ constraint to the 'FieldDefinition'. This constraint
  will be included on any table that uses the field definition.

@since 1.0.0.0
-}
addUniqueConstraint ::
  FieldDefinition nullability a ->
  FieldDefinition nullability a
addUniqueConstraint fieldDef =
  let
    constraintToAdd name =
      ConstraintDefinition.uniqueConstraint (name :| [])
  in
    addFieldTableConstraints [constraintToAdd] fieldDef

{- |
  Marshalls a Haskell value to be stored in the field to its 'SqlValue.SqlValue'
  representation and packages the result as a 'Expr.ValueExpression' so that
  it can be easily used with other @Expr@ functions.

@since 1.0.0.0
-}
fieldValueToExpression :: FieldDefinition nullability a -> a -> Expr.ValueExpression
fieldValueToExpression field =
  Expr.valueExpression . fieldValueToSqlValue field

{- |
  Marshalls a Haskell value to be stored in the field to its 'SqlValue.SqlValue'
  representation.

@since 1.0.0.0
-}
fieldValueToSqlValue :: FieldDefinition nullability a -> a -> SqlValue.SqlValue
fieldValueToSqlValue = SqlType.sqlTypeToSql . fieldType

{- |
  Marshalls a 'SqlValue.SqlValue' from the database into the Haskell value that represents it.
  This may fail, in which case a 'Left' is returned with an error message.

@since 1.0.0.0
-}
fieldValueFromSqlValue :: FieldDefinition nullability a -> SqlValue.SqlValue -> Either String a
fieldValueFromSqlValue =
  SqlType.sqlTypeFromSql . fieldType

{- |
  Constructs the 'Expr.ColumnName' for a field for use in SQL expressions
  from the "Orville.PostgreSQL.Expr" module.

@since 1.0.0.0
-}
fieldColumnName :: Maybe AliasName -> FieldDefinition nullability a -> Expr.Qualified Expr.ColumnName
fieldColumnName mbAlias =
  Expr.aliasQualifyColumn (fmap aliasNameToAliasExpr mbAlias) . fieldNameToColumnName . fieldName

{- |
  Constructs the 'Expr.ValueExpression' for use in SQL expressions from the
  "Orville.PostgreSQL.Expr" module.

@since 1.0.0.0
-}
fieldColumnReference :: FieldDefinition nullability a -> Expr.ValueExpression
fieldColumnReference =
  Expr.columnReference
    . Expr.aliasQualifyColumn Nothing
    . fieldNameToColumnName
    . fieldName

{- |
  Constructs the equivalent 'Expr.FieldDefinition' as a SQL expression,
  generally for use in DDL for creating columns in a table.

@since 1.0.0.0
-}
fieldColumnDefinition :: FieldDefinition nullability a -> Expr.ColumnDefinition
fieldColumnDefinition fieldDef =
  Expr.columnDefinition
    (fieldNameToColumnName $ fieldName fieldDef)
    (SqlType.sqlTypeExpr $ fieldType fieldDef)
    (Just $ fieldColumnConstraint fieldDef)
    (fmap (Expr.columnDefault . DefaultValue.defaultValueExpression) $ i_fieldDefaultValue fieldDef)

{- |
  INTERNAL - Builds the appropriate ColumnConstraint for a field. Currently
  this only handles nullability, but if we add support for more constraints
  directly on columns it may end up handling those as well.

@since 1.0.0.0
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

  The 'NullabilityGADT' represents whether a field will be marked as @NULL@ or
  'NOT NULL' in the database schema. It is a GADT so that the value
  constructors can be used to record this knowledge in the type system as well.
  This allows functions that work only on 'Nullable' or 'NotNull' fields to
  indicate this in their type signatures as appropriate.

@since 1.0.0.0
-}
data NullabilityGADT nullability where
  NullableGADT :: NullabilityGADT Nullable
  NotNullGADT :: NullabilityGADT NotNull

{- |

  'NotNull' is a valueless type used to track that a 'FieldDefinition'
  represents a field that is marked not-null in the database schema. See the
  'FieldNullability' type for the value-level representation of field nullability.

@since 1.0.0.0
-}
data NotNull

{- |
  'Nullable' is a valueless type used to track that a 'FieldDefinition'
  represents a field that is marked nullable in the database schema. See the
  'FieldNullability' type for the value-level representation of field nullability.

@since 1.0.0.0
-}
data Nullable

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int32' values as the
  PostgreSQL "INT" type.

@since 1.0.0.0
-}
integerField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Int32
integerField = fieldOfType SqlType.integer

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int16' values as the
  PostgreSQL "SMALLINT" type.

@since 1.0.0.0
-}
smallIntegerField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Int16
smallIntegerField = fieldOfType SqlType.smallInteger

{- |
  Builds a 'FieldDefinition' that stores an 'Int32' value as the "SERIAL"
  type. This can be used to create auto-incrementing columns.

@since 1.0.0.0
-}
serialField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Int32
serialField = fieldOfType SqlType.serial

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Int64' values as the
  PostgreSQL "BIGINT" type.

@since 1.0.0.0
-}
bigIntegerField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Int64
bigIntegerField = fieldOfType SqlType.bigInteger

{- |
  Builds a 'FieldDefinition' that stores an 'Int64' value as the "BIGSERIAL"
  type. This can be used to create auto-incrementing columns.

@since 1.0.0.0
-}
bigSerialField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Int64
bigSerialField = fieldOfType SqlType.bigSerial

{- |
  Builds a 'FieldDefinition' that stores a 'Double' value as the "DOUBLE
  PRECISION" type. Note: PostgreSQL's "DOUBLE PRECISION" type only allows for
  up to 15 digits of precision, so some rounding may occur when values are
  stored in the database.

@since 1.0.0.0
-}
doubleField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Double
doubleField = fieldOfType SqlType.double

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Bool' values as the
  PostgreSQL "BOOLEAN" type.

@since 1.0.0.0
-}
booleanField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Bool
booleanField = fieldOfType SqlType.boolean

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "TEXT" type. Note that this PostgreSQL has no particular
  limit on the length of text stored.

@since 1.0.0.0
-}
unboundedTextField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull T.Text
unboundedTextField = fieldOfType SqlType.unboundedText

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "VARCHAR" type. Attempting to store a value beyond the length
  specified will cause an error.

@since 1.0.0.0
-}
boundedTextField ::
  -- | Name of the field in the database.
  String ->
  -- | Maximum length of text in the field.
  Int32 ->
  FieldDefinition NotNull T.Text
boundedTextField name len = fieldOfType (SqlType.boundedText len) name

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "CHAR" type. Attempting to store a value beyond the length
  specified will cause an error. Storing a value that is not the full
  length of the field will result in padding by the database.

@since 1.0.0.0
-}
fixedTextField ::
  -- | Name of the field in the database.
  String ->
  -- | Maximum length of text in the field.
  Int32 ->
  FieldDefinition NotNull T.Text
fixedTextField name len = fieldOfType (SqlType.fixedText len) name

{- |
  Builds a @FieldDefinition@ that stores PostgreSQL text search vector values.
  The values are represented as Haskell 'T.Text' values, but are interpreted as
  text search vector values by PostgreSQL when passed to it.

  See https://www.postgresql.org/docs/current/datatype-textsearch.html for
  information about how PostgreSQL creates @tsvector@ values from strings.

@since 1.0.0.0
-}
textSearchVectorField :: String -> FieldDefinition NotNull T.Text
textSearchVectorField = fieldOfType SqlType.textSearchVector

{- |
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "JSONB" type.

@since 1.0.0.0
-}
jsonbField ::
  String ->
  FieldDefinition NotNull T.Text
jsonbField = fieldOfType SqlType.jsonb

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.Day' values as the
  PostgreSQL "DATE" type.

@since 1.0.0.0
-}
dateField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Time.Day
dateField = fieldOfType SqlType.date

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.UTCTime' values as the
  PostgreSQL "TIMESTAMP with time zone" type.

@since 1.0.0.0
-}
utcTimestampField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Time.UTCTime
utcTimestampField = fieldOfType SqlType.timestamp

{- |
  Builds a 'FieldDefinition' that stores Haskell 'Time.UTCTime' values as the
  PostgreSQL "TIMESTAMP without time zone" type.

@since 1.0.0.0
-}
localTimestampField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull Time.LocalTime
localTimestampField = fieldOfType SqlType.timestampWithoutZone

{- |
  Builds a 'FieldDefinition' that stores Haskell 'UUID.UUID' values as the
  PostgreSQL "UUID" type.

@since 1.0.0.0
-}
uuidField ::
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull UUID.UUID
uuidField = fieldOfType SqlType.uuid

{- |
  Builds a 'FieldDefinition' that will use the given 'SqlType.SqlType' to
  determine the database representation of the field. If you have created a
  custom 'SqlType.SqlType', you can use this function to construct a helper
  like the other functions in this module for creating 'FieldDefinition's for
  your custom type.

@since 1.0.0.0
-}
fieldOfType ::
  -- | 'SqlType.SqlType' that represents the PostgreSQL data type for the field.
  SqlType.SqlType a ->
  -- | Name of the field in the database.
  String ->
  FieldDefinition NotNull a
fieldOfType sqlType name =
  FieldDefinition
    { i_fieldName = stringToFieldName name
    , i_fieldType = sqlType
    , i_fieldNullability = NotNullGADT
    , i_fieldDefaultValue = Nothing
    , i_fieldDescription = Nothing
    , i_fieldTableConstraints = mempty
    }

{- |
  Makes a 'NotNull' field 'Nullable' by wrapping the Haskell type of the field
  in 'Maybe'. The field will be marked as @NULL@ in the database schema and
  the value 'Nothing' will be used to represent @NULL@ values when converting
  to and from SQL.

@since 1.0.0.0
-}
nullableField :: FieldDefinition NotNull a -> FieldDefinition Nullable (Maybe a)
nullableField field =
  let
    nullableType :: SqlType.SqlType a -> SqlType.SqlType (Maybe a)
    nullableType sqlType =
      sqlType
        { SqlType.sqlTypeToSql = maybe SqlValue.sqlNull (SqlType.sqlTypeToSql sqlType)
        , SqlType.sqlTypeFromSql =
            \sqlValue ->
              if SqlValue.isSqlNull sqlValue
                then Right Nothing
                else Just <$> SqlType.sqlTypeFromSql sqlType sqlValue
        }
  in
    FieldDefinition
      { i_fieldName = fieldName field
      , i_fieldType = nullableType (fieldType field)
      , i_fieldNullability = NullableGADT
      , i_fieldDefaultValue = fmap DefaultValue.coerceDefaultValue (i_fieldDefaultValue field)
      , i_fieldDescription = fieldDescription field
      , i_fieldTableConstraints = i_fieldTableConstraints field
      }

{- |
  Adds a 'Maybe' wrapper to a field that is already nullable. (If your field is
  'NotNull', you wanted 'nullableField' instead of this function). Note that
  fields created using this function have asymmetric encoding and decoding of
  @NULL@ values. Because the provided field is 'Nullable', @NULL@ values decoded
  from the database already have a representation in the @a@ type, so @NULL@
  will be decoded as 'Just <value of type a for NULL>'. This means if you
  insert a 'Nothing' value using the field, it will be read back as 'Just'
  value. This is useful for building high level combinators that might need to
  make fields 'Nullable' but need the value to be decoded in its underlying
  type when reading back (e.g. 'Orville.PostgreSQL.maybeMapper' from
  "Orville.PostgreSQL.Marshall.SqlMarshaller").

@since 1.0.0.0
-}
asymmetricNullableField :: FieldDefinition Nullable a -> FieldDefinition Nullable (Maybe a)
asymmetricNullableField field =
  let
    nullableType :: SqlType.SqlType a -> SqlType.SqlType (Maybe a)
    nullableType sqlType =
      sqlType
        { SqlType.sqlTypeToSql = maybe SqlValue.sqlNull (SqlType.sqlTypeToSql sqlType)
        , SqlType.sqlTypeFromSql = \sqlValue -> Just <$> SqlType.sqlTypeFromSql sqlType sqlValue
        }
  in
    FieldDefinition
      { i_fieldName = fieldName field
      , i_fieldType = nullableType (fieldType field)
      , i_fieldNullability = NullableGADT
      , i_fieldDefaultValue = fmap DefaultValue.coerceDefaultValue (i_fieldDefaultValue field)
      , i_fieldDescription = fieldDescription field
      , i_fieldTableConstraints = i_fieldTableConstraints field
      }

{- |
  Applies a 'SqlType.SqlType' conversion to a 'FieldDefinition'. You can
  use this function to create 'FieldDefinition's based on the primitive ones
  provided, but with more specific Haskell types.

  See 'SqlType.convertSqlType' and 'SqlType.tryConvertSqlType' for functions
  to create the conversion needed as the first argument to 'convertField'.

@since 1.0.0.0
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
  'Coerce.Coercible'. This is particularly useful for newtype wrappers around
  primitive types.

@since 1.0.0.0
-}
coerceField ::
  Coerce.Coercible a b =>
  FieldDefinition nullability a ->
  FieldDefinition nullability b
coerceField =
  convertField
    (SqlType.convertSqlType Coerce.coerce Coerce.coerce)

{- |
  Sets a default value for the field. The default value will be added as part
  of the column definition in the database. Because the default value is
  ultimately provided by the database, this can be used to add a not-null column
  safely to an existing table as long as a reasonable default value is
  available to use.

@since 1.0.0.0
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

@since 1.0.0.0
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

@since 1.0.0.0
-}
prefixField ::
  String ->
  FieldDefinition nullability a ->
  FieldDefinition nullability a
prefixField prefix fieldDef =
  fieldDef
    { i_fieldName = byteStringToFieldName (B8.pack prefix <> "_" <> fieldNameToByteString (fieldName fieldDef))
    }

{- |
  Constructs a 'Expr.SetClause' that will set the column named in the
  field definition to the given value. The value is converted to a SQL
  value using 'fieldValueToSqlValue'.

@since 1.0.0.0
-}
setField :: FieldDefinition nullability a -> a -> Expr.SetClause
setField fieldDef =
  Expr.setColumn
    (fieldColumnName Nothing fieldDef)
    . fieldValueToSqlValue fieldDef

{- |
  Operator alias for 'setField'.

@since 1.0.0.0
-}
(.:=) :: FieldDefinition nullability a -> a -> Expr.SetClause
(.:=) = setField

{- |
  Checks that the value in a field equals a particular value.

@since 1.0.0.0
-}
fieldEquals :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldEquals = SqlComparable.equals

{- |
  Operator alias for 'fieldEquals'.

@since 1.0.0.0
-}
(.==) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.==) = fieldEquals

infixl 9 .==

{- |
  Checks that the value in a field does not equal a particular value.

@since 1.0.0.0
-}
fieldNotEquals :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldNotEquals = SqlComparable.notEquals

{- |
  Operator alias for 'fieldNotEquals'.

@since 1.0.0.0
-}
(./=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(./=) = fieldNotEquals

infixl 9 ./=

{- |
  Checks that the value in a field is distinct from a particular value.

@since 1.1.0.0
-}
fieldIsDistinctFrom :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldIsDistinctFrom = SqlComparable.isDistinctFrom

{- |
  Checks that the value in a field is not distinct from a particular value.

@since 1.1.0.0
-}
fieldIsNotDistinctFrom :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldIsNotDistinctFrom = SqlComparable.isNotDistinctFrom

{- |
  Checks that the value in a field is greater than a particular value.

@since 1.0.0.0
-}
fieldGreaterThan :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldGreaterThan = SqlComparable.greaterThan

{- |
  Operator alias for 'fieldGreaterThan'.

@since 1.0.0.0
-}
(.>) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.>) = fieldGreaterThan

infixl 9 .>

{- |
  Checks that the value in a field is less than a particular value.

@since 1.0.0.0
-}
fieldLessThan :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldLessThan = SqlComparable.lessThan

{- |
  Operator alias for 'fieldLessThan'.

@since 1.0.0.0
-}
(.<) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.<) = fieldLessThan

infixl 9 .<

{- |
  Checks that the value in a field is greater than or equal to a particular value.

@since 1.0.0.0
-}
fieldGreaterThanOrEqualTo :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldGreaterThanOrEqualTo = SqlComparable.greaterThanOrEqualTo

{- |
  Operator alias for 'fieldGreaterThanOrEqualTo'.

@since 1.0.0.0
-}
(.>=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.>=) = fieldGreaterThanOrEqualTo

infixl 9 .>=

{- |
  Checks that the value in a field is less than or equal to a particular value.

@since 1.0.0.0
-}
fieldLessThanOrEqualTo :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
fieldLessThanOrEqualTo = SqlComparable.lessThanOrEqualTo

{- |
  Operator alias for 'fieldLessThanOrEqualTo'.

@since 1.0.0.0
-}
(.<=) :: FieldDefinition nullability a -> a -> Expr.BooleanExpr
(.<=) = fieldLessThanOrEqualTo

infixl 9 .<=

{- |
  Checks that the value in a field matches a like pattern.

@since 1.0.0.0
-}
fieldLike :: FieldDefinition nullability a -> T.Text -> Expr.BooleanExpr
fieldLike = SqlComparable.like

{- |
  Checks that the value in a field matches a like pattern case insensitively.

@since 1.0.0.0
-}
fieldLikeInsensitive :: FieldDefinition nullability a -> T.Text -> Expr.BooleanExpr
fieldLikeInsensitive = SqlComparable.likeInsensitive

{- |
  Checks that the value in a field is null.

@since 1.0.0.0
-}
fieldIsNull :: FieldDefinition Nullable a -> Expr.BooleanExpr
fieldIsNull = SqlComparable.isNull

{- |
  Checks that the value in a field is not null.

@since 1.0.0.0
-}
fieldIsNotNull :: FieldDefinition Nullable a -> Expr.BooleanExpr
fieldIsNotNull = SqlComparable.isNotNull

{- |
  Checks that a field matches a list of values.

@since 1.0.0.0
-}
fieldIn :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
fieldIn = SqlComparable.isIn

{- |
  Operator alias for 'fieldIn'.

@since 1.0.0.0
-}
(.<-) :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
(.<-) = fieldIn

infixl 9 .<-

{- |
  Checks that a field does not match a list of values.

@since 1.0.0.0
-}
fieldNotIn :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
fieldNotIn = SqlComparable.isNotIn

{- |
  Operator alias for 'fieldNotIn'.

@since 1.0.0.0
-}
(.</-) :: FieldDefinition nullability a -> NonEmpty a -> Expr.BooleanExpr
(.</-) = fieldNotIn

infixl 9 .</-

{- |
  Checks that a tuple of two fields is in the list of specified tuples.

@since 1.0.0.0
-}
fieldTupleIn ::
  FieldDefinition nullabilityA a ->
  FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  Expr.BooleanExpr
fieldTupleIn =
  SqlComparable.tupleIn

{- |
  Checks that a tuple of two fields is not in the list of specified tuples.

@since 1.0.0.0
-}
fieldTupleNotIn ::
  FieldDefinition nullabilityA a ->
  FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  Expr.BooleanExpr
fieldTupleNotIn = SqlComparable.tupleNotIn

{- |
  Constructs a field-based 'Expr.BooleanExpr' using a function that
  builds a 'Expr.BooleanExpr'.

@since 1.0.0.0
-}
whereColumnComparison ::
  (Expr.ValueExpression -> Expr.ValueExpression -> Expr.BooleanExpr) ->
  (FieldDefinition nullability a -> a -> Expr.BooleanExpr)
whereColumnComparison columnComparison fieldDef =
  columnComparison
    (SqlComparable.referenceValueExpression fieldDef)
    . fieldValueToExpression fieldDef

{- |
  Orders a query by the column name for the given field.

@since 1.0.0.0
-}
orderByField ::
  FieldDefinition nullability value ->
  Expr.OrderByDirection ->
  Expr.OrderByExpr
orderByField =
  Expr.orderByColumnName . fieldColumnName Nothing

{- | Orders a query by the column name for the given field, taking into account the alias, if any.

@since 1.1.0.0
-}
orderByAliasedField ::
  AliasedFieldDefinition nullability value ->
  Expr.OrderByDirection ->
  Expr.OrderByExpr
orderByAliasedField aliasedFieldDef =
  Expr.orderByColumnName (fieldColumnName (getAlias aliasedFieldDef) (getFieldDefinition aliasedFieldDef))

{- | A 'FieldDefinition' that might have been aliased. This is equivalent to a tuple of the field
definition and 'Maybe Expr.Alias'.

@since 1.1.0.0
-}
data AliasedFieldDefinition nullability a = AliasedFieldDefinition
  { i_alias :: Maybe AliasName
  , i_fieldDef :: FieldDefinition nullability a
  }

{- |

@since 1.1.0.0
-}
instance SqlComparable.SqlComparable (AliasedFieldDefinition nullability a) a where
  toComparableSqlValue (AliasedFieldDefinition _ fieldDef) =
    toComparableSqlValue fieldDef

  referenceValueExpression (AliasedFieldDefinition mbAlias fieldDef) =
    Expr.columnReference $
      fieldColumnName mbAlias fieldDef

{- |
Obtains the alias used with the field definition.

@since 1.1.0.0
-}
getAlias :: AliasedFieldDefinition nullability a -> Maybe AliasName
getAlias = i_alias

{- |
Obtains the field definition as it was prior to being aliased.

@since 1.1.0.0
-}
getFieldDefinition :: AliasedFieldDefinition nullability a -> FieldDefinition nullability a
getFieldDefinition = i_fieldDef

{- |
Alias an existing field definition.

@since 1.1.0.0
-}
buildAliasedFieldDefinition :: FieldDefinition nullability a -> Maybe AliasName -> AliasedFieldDefinition nullability a
buildAliasedFieldDefinition f ma =
  AliasedFieldDefinition ma f
