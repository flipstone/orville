{-|
Module    : Database.Orville.PostgreSQL.Expr.FieldDefinition
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.FieldDefinition
  ( FieldDefinition
  , fieldDefinition
  , fieldDefinitionToSql
  , DataType
  , timestampWithZone
  , date
  , tsvector
  , varchar
  , char
  , text
  , boolean
  , doublePrecision
  , bigSerial
  , bigInt
  , serial
  , int
  ) where

import Database.Orville.PostgreSQL.Internal.Expr.Name
  ( ColumnName
  , columnNameToSql
  )
import Database.Orville.PostgreSQL.Internal.RawSql
  ( RawSql
  , fromString
  )

newtype FieldDefinition =
  FieldDefinition RawSql

fieldDefinitionToSql :: FieldDefinition -> RawSql
fieldDefinitionToSql (FieldDefinition sql) = sql

fieldDefinition :: ColumnName -> DataType -> FieldDefinition
fieldDefinition columnName dataType =
  FieldDefinition $
    columnNameToSql columnName
    <> fromString " "
    <> dataTypeToSql dataType

newtype DataType =
  DataType RawSql

dataTypeToSql :: DataType -> RawSql
dataTypeToSql (DataType sql) = sql

timestampWithZone :: DataType
timestampWithZone =
  DataType (fromString "TIMESTAMP with time zone")

date :: DataType
date =
  DataType (fromString "DATE")

tsvector :: DataType
tsvector =
  DataType (fromString "TSVECTOR")

varchar :: Int -> DataType
varchar len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo VARCHAR($1))
  DataType $
    fromString "VARCHAR("
    <> fromString (show len)
    <> fromString ")"

char :: Int -> DataType
char len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo CHAR($1))
  DataType $
    fromString "CHAR("
    <> fromString (show len)
    <> fromString ")"

text :: DataType
text =
  DataType (fromString "TEXT")

boolean :: DataType
boolean =
  DataType (fromString "BOOLEAN")

doublePrecision :: DataType
doublePrecision =
  DataType (fromString "DOUBLE PRECISION")

bigSerial :: DataType
bigSerial =
  DataType (fromString "BIGSERIAL")

bigInt :: DataType
bigInt =
  DataType (fromString "BIGINT")

serial :: DataType
serial =
  DataType (fromString "SERIAL")

int :: DataType
int =
  DataType (fromString "INT")
