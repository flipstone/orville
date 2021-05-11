{-|
Module    : Database.Orville.PostgreSQL.Internal.FieldDefinition
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
{-# LANGUAGE GADTs #-}
module Database.Orville.PostgreSQL.Internal.FieldDefinition
  ( FieldDefinition
  , fieldName
  , fieldType
  , fieldNullability
  , fieldValueToSqlValue
  , fieldValueFromSqlValue
  , fieldColumnName
  , toSqlExpr
  , NotNull
  , Nullable
  , Nullability(..)
  , integerField
  , serialField
  , bigIntegerField
  , bigSerialField
  , doubleField
  , booleanField
  , unboundedTextField
  , boundedTextField
  , fixedTextField
  , textSearchVectorField
  , dateField
  , timestampField
  , fieldOfType
  ) where

import           Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Time as Time

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import           Database.Orville.PostgreSQL.Internal.SqlType (SqlType)
import qualified Database.Orville.PostgreSQL.Internal.SqlType as SqlType
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)

{-|
  'FieldDefinition' determines the SQL constsruction of a column in the
  database, comprising the name, SQL type and whether the field is nullable.
  A 'FieldDefinition' is matched to a particular Haskell type, which it knows
  how to marshall to and from the database representation of SQL type for
  the field.
-}
data FieldDefinition nullability a =
  FieldDefinition
    { _fieldName        :: String
    , _fieldType        :: SqlType a
    , _fieldNullability :: Nullability nullability
    }

{-|
  The name used in database queries to reference the field.
-}
fieldName :: FieldDefinition nullability a -> String
fieldName = _fieldName

{-|
  The 'SqlType' for the 'FieldDefinition' determines the PostgreSQL data type
  used to define the field as well as how to mashall Haskell values to and
  from the database.
-}
fieldType :: FieldDefinition nullability a -> SqlType a
fieldType = _fieldType

{-|
  The 'Nullability' for a field indicates whether the column is marked nullable
  in the database.

  TODO: This is not yet completely ported. I've included it in the initial
  port simply for the sake of having the nullability type parameter.
-}
fieldNullability :: FieldDefinition nullability a -> Nullability nullability
fieldNullability = _fieldNullability

{-|
  Mashalls a Haskell value to be stored in the field to its 'SqlValue'
  representation.
-}
fieldValueToSqlValue :: FieldDefinition nullability a -> a -> SqlValue
fieldValueToSqlValue =
  SqlType.sqlTypeToSql . fieldType

{-|
  Marshalls a 'SqlValue' from the database into the Haskell value that represents it.
  This may fail, in which case 'Nothing' is returned.
-}
fieldValueFromSqlValue :: FieldDefinition nullability a -> SqlValue -> Maybe a
fieldValueFromSqlValue =
  SqlType.sqlTypeFromSql . fieldType

{-|
  Constructs the 'Expr.ColumnName' for a field for use in SQL expressions
  from the 'Expr' module.
-}
fieldColumnName :: FieldDefinition nullability a -> Expr.ColumnName
fieldColumnName =
  Expr.rawColumnName . fieldName

{-|
  Constructions the equivalant 'Expr.FieldDefinition' as a SQL expression,
  generally for use in DDL for creating column in a table.

  TODO: this is NotNull at the momentbecause I haven't made it handle adding the
  NULL modifier to the DDL yet
-}
toSqlExpr :: FieldDefinition NotNull a -> Expr.FieldDefinition
toSqlExpr fieldDef =
  Expr.fieldDefinition
    (fieldColumnName fieldDef)
    (SqlType.sqlTypeExpr $ fieldType fieldDef)

{-|
  'Nullability' represents whether a field will be marked as 'NULL' or 'NOT
  NULL' in the database schema. It is a GADT so that the value constructors
  can be used to record this knowledge in the type system as well. This allows
  functions that work only on 'Nullable' or 'NotNull' fields to indicate this
  in their type signatures as appropriate.
-}
data Nullability nullability where
  Nullable :: Nullability Nullable
  NotNull  :: Nullability NotNull

{-|
  'NotNull' is a values-less type used to track that a 'FieldDefinition'
  represents a field that is marked not-null in the database schema.  See the
  'Nullability' type for the value-level representation of field nullability.
-}
data NotNull

{-|
  'Nullable' is a values-less type used to track that a 'FieldDefinition'
  represents a field that is marked nullable in the database schema. See the
  'Nullability' type for the value-level representation of field nullability.
-}
data Nullable

{-|
  Builds a 'FieldDefinition' that stores Haskell 'Int32' values as the
  PostgreSQL "INT" type.
-}
integerField :: String -- ^ Name of the field in the database
             -> FieldDefinition NotNull Int32
integerField = fieldOfType SqlType.integer

{-|
  Builds a 'FieldDefinition' that stores an 'Int32' value as the "SERIAL"
  type. This can be used to create auto-incrementing columns.
-}
serialField :: String -- ^ Name of the field in the database
            -> FieldDefinition NotNull Int32
serialField = fieldOfType SqlType.serial

{-|
  Builds a 'FieldDefinition' that stores Haskell 'Int64' values as the
  PostgreSQL "BIGINT" type.
-}
bigIntegerField :: String -- ^ Name of the field in the database
                -> FieldDefinition NotNull Int64
bigIntegerField = fieldOfType SqlType.bigInteger

{-|
  Builds a 'FieldDefinition' that stores an 'Int64' value as the "BIGSERIAL"
  type. This can be used to create auto-incrementing columns.
-}
bigSerialField :: String -- ^ Name of the field in the database
               -> FieldDefinition NotNull Int64
bigSerialField = fieldOfType SqlType.bigSerial

{-|
  Builds a 'FieldDefinition' that stores a 'Double' value as the "DOUBLE
  PRECISION" type. Note: PostgreSQL's "DOUBLE PRECISION" type only allows for
  up to 15 digits of precision, so some rounding may occur when values are
  stored in the database.
-}
doubleField :: String -- ^ Name of the field in the database
            -> FieldDefinition NotNull Double
doubleField = fieldOfType SqlType.double

{-|
  Builds a 'FieldDefinition' that stores Haskell 'Bool' values as the
  PostgreSQL "BOOLEAN" type.
-}
booleanField :: String -- ^ Name of the field in the database
             -> FieldDefinition NotNull Bool
booleanField = fieldOfType SqlType.boolean

{-|
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "TEXT" type. Note that this PostgreSQL has no particular
  limit on the length of text stored.
-}
unboundedTextField :: String -- ^ Name of the field in the database
                   -> FieldDefinition NotNull T.Text
unboundedTextField = fieldOfType SqlType.unboundedText

{-|
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "VARCHAR" type. Attempting to store a value beyond the length
  specified will cause an error.

  -- TODO: We should have a test for this.
-}
boundedTextField :: String -- ^ Name of the field in the database
                 -> Int -- ^ Maximum length of text in the field
                 -> FieldDefinition NotNull T.Text
boundedTextField name len = fieldOfType (SqlType.boundedText len) name

{-|
  Builds a 'FieldDefinition' that stores Haskell 'T.Text' values as the
  PostgreSQL "CHAR" type. Attempting to store a value beyond the length
  specified will cause an error. Storing a value that is not the full
  length of the field will result in padding by the database.

  -- TODO: We should have a test for this.
-}
fixedTextField :: String -- ^ Name of the field in the database
               -> Int -- ^ Maximum length of text in the field
               -> FieldDefinition NotNull T.Text
fixedTextField name len = fieldOfType (SqlType.fixedText len) name


{-|
  TODO: write meaningful docs for this when we build a better Haskell
  definition for representing text search vectors.
-}
textSearchVectorField :: String -> FieldDefinition NotNull T.Text
textSearchVectorField = fieldOfType SqlType.textSearchVector

{-|
  Builds a 'FieldDefinition' that stores Haskell 'Time.Day' values as the
  PostgreSQL "DATE" type.
-}
dateField :: String -- ^ Name of the field in the database
          -> FieldDefinition NotNull Time.Day
dateField = fieldOfType SqlType.date

{-|
  Builds a 'FieldDefinition' that stores Haskell 'Time.UTCTime values as the
  PostgreSQL "TIMESTAMP with time zone" type.

  TODO: discuss "TIMESTAMP with zone" to avoid confusion?
-}
timestampField :: String -- ^ Name of the field in the database
               -> FieldDefinition NotNull Time.UTCTime
timestampField = fieldOfType SqlType.timestamp

{-|
  Builds a 'FieldDefinition' for will use the given 'SqlType' to determine
  the database representation of the field. If you have created a custom
  'SqlType', you can use this function to construct a helper like the
  other functions in this module for creating 'FieldDefinition's for your
  custom type.
-}
fieldOfType :: SqlType a -- ^ 'SqlType' that represents the PostgreSQL data type for the field.
            -> String -- ^ Name of the field in the database
            -> FieldDefinition NotNull a
fieldOfType sqlType name =
  FieldDefinition
    name
    sqlType
    NotNull
