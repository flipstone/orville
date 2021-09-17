{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.ColumnDefinition
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.ColumnDefinition
  ( ColumnDefinition,
    columnDefinition,
    ColumnConstraint,
    notNullConstraint,
    nullConstraint,
    DataType,
    timestampWithZone,
    timestampWithoutZone,
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
    smallint,
    oid,
  )
where

import Data.Int (Int32)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnDefinition ::
  ColumnName ->
  DataType ->
  Maybe ColumnConstraint ->
  ColumnDefinition
columnDefinition columnName dataType columnConstraint =
  ColumnDefinition $
    RawSql.toRawSql columnName
      <> RawSql.space
      <> RawSql.toRawSql dataType
      <> RawSql.space
      <> maybe mempty RawSql.toRawSql columnConstraint

newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

newtype DataType
  = DataType RawSql.RawSql
  deriving (RawSql.SqlExpression)

timestampWithZone :: DataType
timestampWithZone =
  DataType (RawSql.fromString "TIMESTAMP with time zone")

timestampWithoutZone :: DataType
timestampWithoutZone =
  DataType (RawSql.fromString "TIMESTAMP without time zone")

date :: DataType
date =
  DataType (RawSql.fromString "DATE")

tsvector :: DataType
tsvector =
  DataType (RawSql.fromString "TSVECTOR")

varchar :: Int32 -> DataType
varchar len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo VARCHAR($1))
  DataType $
    RawSql.fromString "VARCHAR("
      <> RawSql.fromString (show len)
      <> RawSql.fromString ")"

char :: Int32 -> DataType
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

smallint :: DataType
smallint =
  DataType (RawSql.fromString "SMALLINT")

oid :: DataType
oid =
  DataType (RawSql.fromString "OID")
