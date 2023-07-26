{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2023
License   : MIT
-}
module Orville.PostgreSQL.Expr.DataType
  ( DataType
  , timestampWithZone
  , timestampWithoutZone
  , date
  , tsvector
  , varchar
  , char
  , text
  , uuid
  , boolean
  , doublePrecision
  , bigSerial
  , bigInt
  , serial
  , int
  , smallint
  , jsonb
  , oid
  )
where

import Data.Int (Int32)

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent any SQL data type expression.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to have a data type corresponding to the fictional
SQL operator MY_DATA_TYPE, that could be done as

 > RawSql.unsafeSqlExpression "MY_DATA_TYPE"

@since 0.10.0.0
-}
newtype DataType
  = DataType RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

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

jsonb :: DataType
jsonb =
  DataType (RawSql.fromString "JSONB")

oid :: DataType
oid =
  DataType (RawSql.fromString "OID")
