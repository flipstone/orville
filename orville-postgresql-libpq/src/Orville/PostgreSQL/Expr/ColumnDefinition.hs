{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.ColumnDefinition
  ( ColumnDefinition
  , columnDefinition
  , ColumnConstraint
  , notNullConstraint
  , nullConstraint
  , ColumnDefault
  , columnDefault
  )
where

import qualified Data.Maybe as Maybe

import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.Name (ColumnName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Represent a complete definition of a column.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is required but not
already included. The extension mechanism provided does require care in use as no guarantees are
provided for correctness in usage.

For example, if one wanted to have a column named FOO of type INT, that could be done as

 > RawSql.unsafeSqlExpression "FOO INT"

@since 0.10.0.0
-}
newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- | Smart constructor for ensuring a 'ColumnDefinition' is setup correctly.

@since 0.10.0.0
-}
columnDefinition ::
  -- | The name the resulting column should have.
  ColumnName ->
  -- | The SQL type of the column
  DataType ->
  -- | The constraint on the column if any
  Maybe ColumnConstraint ->
  -- | The default value for the column if any
  Maybe ColumnDefault ->
  ColumnDefinition
columnDefinition columnName dataType maybeColumnConstraint maybeColumnDefault =
  ColumnDefinition
    . RawSql.intercalate RawSql.space
    $ Maybe.catMaybes
      [ Just $ RawSql.toRawSql columnName
      , Just $ RawSql.toRawSql dataType
      , fmap RawSql.toRawSql maybeColumnConstraint
      , fmap RawSql.toRawSql maybeColumnDefault
      ]

{- | Represent constraints, such as nullability, on a column.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is required but not
already included. The extension mechanism provided does require care in use as no guarantees are
provided for correctness in usage.

For example, if one wanted to have a constraint of NOT NULL, that could be done as

 > RawSql.unsafeSqlExpression "NOT NULL"

@since 0.10.0.0
-}
newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- | Express that a column may not contain NULL.

@since 0.10.0.0
-}
notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

{- | Express that a column may contain NULL.

@since 0.10.0.0
-}
nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

{- | Represent the default value of a column.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is required but not
already included. The extension mechanism provided does require care in use as no guarantees are
provided for correctness in usage.

For example, if one wanted to have a default of FOO, that could be done as

 > RawSql.unsafeSqlExpression "FOO"

@since 0.10.0.0
-}
newtype ColumnDefault
  = ColumnDefault RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- | Given a 'ValueExpression' use that as a 'ColumnDefault'. This is the preferred path to creating a
   column default. Note that it is up to the caller to ensure the 'ValueExpression' makes sense for
   the resulting 'ColumnDefinition' this will be a part of.

@since 0.10.0.0
-}
columnDefault ::
  ValueExpression ->
  ColumnDefault
columnDefault defaultValue =
  ColumnDefault (RawSql.fromString "DEFAULT " <> RawSql.toRawSql defaultValue)
