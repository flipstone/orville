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
    ColumnDefault,
    columnDefault,
    DataType,
    timestampWithZone,
    timestampWithoutZone,
    date,
    tsvector,
    varchar,
    char,
    text,
    uuid,
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
import qualified Data.Maybe as Maybe

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import Orville.PostgreSQL.Internal.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnDefinition ::
  ColumnName ->
  DataType ->
  Maybe ColumnConstraint ->
  Maybe ColumnDefault ->
  ColumnDefinition
columnDefinition columnName dataType maybeColumnConstraint maybeColumnDefault =
  ColumnDefinition $
    RawSql.intercalate RawSql.space $
      Maybe.catMaybes
        [ Just $ RawSql.toRawSql columnName
        , Just $ RawSql.toRawSql dataType
        , fmap RawSql.toRawSql maybeColumnConstraint
        , fmap RawSql.toRawSql maybeColumnDefault
        ]

newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

newtype ColumnDefault
  = ColumnDefault RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnDefault ::
  ValueExpression ->
  ColumnDefault
columnDefault defaultValue =
  ColumnDefault (RawSql.fromString "DEFAULT " <> RawSql.toRawSql defaultValue)

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
      <> RawSql.int32DecLiteral len
      <> RawSql.fromString ")"

char :: Int32 -> DataType
char len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo CHAR($1))
  DataType $
    RawSql.fromString "CHAR("
      <> RawSql.int32DecLiteral len
      <> RawSql.fromString ")"

text :: DataType
text =
  DataType (RawSql.fromString "TEXT")

uuid :: DataType
uuid =
  DataType (RawSql.fromString "UUID")

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
