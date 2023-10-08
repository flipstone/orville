{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
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
Type to represent any SQL data type expression. E.G.

  > INTEGER

'DataType' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DataType
  = DataType RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | A 'DataType' that represents the PostgreSQL "TIMESTAMP with time zone" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-datetime.html) for
more information.

@since 1.0.0.0
-}
timestampWithZone :: DataType
timestampWithZone =
  DataType (RawSql.fromString "TIMESTAMP with time zone")

{- | A 'DataType' that represents the PostgreSQL "TIMESTAMP without time zone" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-datetime.html) for
more information.

@since 1.0.0.0
-}
timestampWithoutZone :: DataType
timestampWithoutZone =
  DataType (RawSql.fromString "TIMESTAMP without time zone")

{- | A 'DataType' that represents the PostgreSQL "DATE" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-datetime.html) for
more information.

@since 1.0.0.0
-}
date :: DataType
date =
  DataType (RawSql.fromString "DATE")

{- | A 'DataType' that represents the PostgreSQL "TSVECTOR" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-textsearch.html#DATATYPE-TSVECTOR)
for more information.

@since 1.0.0.0
-}
tsvector :: DataType
tsvector =
  DataType (RawSql.fromString "TSVECTOR")

{- | A 'DataType' that represents the PostgreSQL "VARCHAR(n)" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-character.html) for
more information.

@since 1.0.0.0
-}
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

{- | A 'DataType' that represents the PostgreSQL "CHAR(n)" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-character.html) for
more information.

@since 1.0.0.0
-}
char :: Int32 -> DataType
char len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try, we get an error like such:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo CHAR($1))
  DataType $
    RawSql.fromString "CHAR("
      <> RawSql.int32DecLiteral len
      <> RawSql.fromString ")"

{- | A 'DataType' that represents the PostgreSQL "TEXT" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-character.html) for
more information.

@since 1.0.0.0
-}
text :: DataType
text =
  DataType (RawSql.fromString "TEXT")

{- | A 'DataType' that represents the PostgreSQL "UUID" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-uuid.html) for more
information.

@since 1.0.0.0
-}
uuid :: DataType
uuid =
  DataType (RawSql.fromString "UUID")

{- | A 'DataType' that represents the PostgreSQL "BOOLEAN" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-boolean.html) for
more information.

@since 1.0.0.0
-}
boolean :: DataType
boolean =
  DataType (RawSql.fromString "BOOLEAN")

{- | A 'DataType' that represents the PostgreSQL "DOUBLE PRECISION" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-FLOAT) for
more information.

@since 1.0.0.0
-}
doublePrecision :: DataType
doublePrecision =
  DataType (RawSql.fromString "DOUBLE PRECISION")

{- | A 'DataType' that represents the PostgreSQL "BIGSERIAL" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-SERIAL) for
more information.

@since 1.0.0.0
-}
bigSerial :: DataType
bigSerial =
  DataType (RawSql.fromString "BIGSERIAL")

{- | A 'DataType' that represents the PostgreSQL "BIGINT" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-INT) for
more information.

@since 1.0.0.0
-}
bigInt :: DataType
bigInt =
  DataType (RawSql.fromString "BIGINT")

{- | A 'DataType' that represents the PostgreSQL "SERIAL" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-SERIAL) for
more information.

@since 1.0.0.0
-}
serial :: DataType
serial =
  DataType (RawSql.fromString "SERIAL")

{- | A 'DataType' that represents the PostgreSQL "INT" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-INT) for
more information.

@since 1.0.0.0
-}
int :: DataType
int =
  DataType (RawSql.fromString "INT")

{- | A 'DataType' that represents the PostgreSQL "SMALLINT" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-numeric.html#DATATYPE-INT) for
more information.

@since 1.0.0.0
-}
smallint :: DataType
smallint =
  DataType (RawSql.fromString "SMALLINT")

{- |
  A 'DataType' that represents the PostgreSQL "JSONB" data type.

See [postgresql documentation](https://www.postgresql.org/docs/current/datatype-json.html) for more
information.

@since 1.0.0.0
-}
jsonb :: DataType
jsonb =
  DataType (RawSql.fromString "JSONB")

{- | A 'DataType' that represents the PostgreSQL "OID" data type.

See [postgresql
documentation](https://www.postgresql.org/docs/current/datatype-oid.html) for
more information.

@since 1.0.0.0
-}
oid :: DataType
oid =
  DataType (RawSql.fromString "OID")
