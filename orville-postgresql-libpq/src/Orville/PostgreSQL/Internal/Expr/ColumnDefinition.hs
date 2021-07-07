{- |
Module    : Orville.PostgreSQL.Expr.ColumnDefinition
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.ColumnDefinition
  ( ColumnDefinition,
    columnDefinition,
    columnDefinitionToSql,
    ColumnConstraint,
    columnConstraintToSql,
    notNullConstraint,
    nullConstraint,
    DataType,
    timestampWithZone,
    date,
    tsvector,
    varchar,
    char,
    text,
    boolean,
    doublePrecision,
    bigSerial,
    bigInt,
    serial,
    int,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql

columnDefinitionToSql :: ColumnDefinition -> RawSql.RawSql
columnDefinitionToSql (ColumnDefinition sql) = sql

columnDefinition ::
  ColumnName ->
  DataType ->
  Maybe ColumnConstraint ->
  ColumnDefinition
columnDefinition columnName dataType columnConstraint =
  ColumnDefinition $
    RawSql.toRawSql columnName
      <> RawSql.space
      <> dataTypeToSql dataType
      <> RawSql.space
      <> maybe mempty columnConstraintToSql columnConstraint

newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql

columnConstraintToSql :: ColumnConstraint -> RawSql.RawSql
columnConstraintToSql (ColumnConstraint sql) = sql

notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

newtype DataType
  = DataType RawSql.RawSql

dataTypeToSql :: DataType -> RawSql.RawSql
dataTypeToSql (DataType sql) = sql

timestampWithZone :: DataType
timestampWithZone =
  DataType (RawSql.fromString "TIMESTAMP with time zone")

date :: DataType
date =
  DataType (RawSql.fromString "DATE")

tsvector :: DataType
tsvector =
  DataType (RawSql.fromString "TSVECTOR")

varchar :: Int -> DataType
varchar len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo VARCHAR($1))
  DataType $
    RawSql.fromString "VARCHAR("
      <> RawSql.fromString (show len)
      <> RawSql.fromString ")"

char :: Int -> DataType
char len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo CHAR($1))
  DataType $
    RawSql.fromString "CHAR("
      <> RawSql.fromString (show len)
      <> RawSql.fromString ")"

text :: DataType
text =
  DataType (RawSql.fromString "TEXT")

boolean :: DataType
boolean =
  DataType (RawSql.fromString "BOOLEAN")

doublePrecision :: DataType
doublePrecision =
  DataType (RawSql.fromString "DOUBLE PRECISION")

bigSerial :: DataType
bigSerial =
  DataType (RawSql.fromString "BIGSERIAL")

bigInt :: DataType
bigInt =
  DataType (RawSql.fromString "BIGINT")

serial :: DataType
serial =
  DataType (RawSql.fromString "SERIAL")

int :: DataType
int =
  DataType (RawSql.fromString "INT")
