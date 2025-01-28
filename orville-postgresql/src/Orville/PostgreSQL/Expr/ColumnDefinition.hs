{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.ColumnDefinition
  ( ColumnDefinition
  , columnDefinition
  , ColumnConstraint
  , notNullConstraint
  , nullConstraint
  , ColumnDefault
  , columnDefault
  , identityColumnConstraint
  , ColumnIdentityGeneration
  , alwaysColumnIdentityGeneration
  , byDefaultColumnIdentityGeneration
  )
where

import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.Name (ColumnName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Represent a complete definition of a column. E.G.

> foo INTEGER

'ColumnDefinition' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Smart constructor for ensuring a 'ColumnDefinition' is set up correctly.

@since 1.0.0.0
-}
columnDefinition ::
  -- | The name the resulting column should have.
  ColumnName ->
  -- | The SQL type of the column.
  DataType ->
  -- | The constraints on the column, if any.
  [ColumnConstraint] ->
  -- | The default value for the column, if any.
  Maybe ColumnDefault ->
  ColumnDefinition
columnDefinition columnName dataType columnConstraints maybeColumnDefault =
  let
    constraintRawSql =
      RawSql.intercalate RawSql.space columnConstraints
  in
    ColumnDefinition $
      RawSql.toRawSql columnName
        <> RawSql.space
        <> RawSql.toRawSql dataType
        <> RawSql.space
        <> constraintRawSql
        <> case maybeColumnDefault of
          Nothing -> mempty
          Just colDefault ->
            RawSql.space <> RawSql.toRawSql colDefault

{- | Represent constraints, such as nullability, on a column. E.G.

> NOT NULL

'ColumnConstraint' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Express that a column may not contain NULL.

@since 1.0.0.0
-}
notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

{- | Express that a column may contain NULL.

@since 1.0.0.0
-}
nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

{- | Represent the generation definition of an identity column. E.G.

> ALWAYS

'ColumnIdentityGeneration' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype ColumnIdentityGeneration
  = ColumnIdentityGeneration RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Express that a column is an identity column.

@since 1.1.0.0
-}
identityColumnConstraint ::
  ColumnIdentityGeneration ->
  ColumnConstraint
identityColumnConstraint identityGeneration =
  ColumnConstraint $
    RawSql.fromString "GENERATED "
      <> RawSql.toRawSql identityGeneration
      <> RawSql.fromString " AS IDENTITY"

{- | The @ALWAYS@ generation for an identity column

@since 1.1.0.0
-}
alwaysColumnIdentityGeneration :: ColumnIdentityGeneration
alwaysColumnIdentityGeneration = ColumnIdentityGeneration $ RawSql.fromString "ALWAYS"

{- | The @BY DEFAULT@ generation for an identity column

@since 1.1.0.0
-}
byDefaultColumnIdentityGeneration :: ColumnIdentityGeneration
byDefaultColumnIdentityGeneration = ColumnIdentityGeneration $ RawSql.fromString "BY DEFAULT"

{- | Represents the default value of a column. E.G.

> now()

'ColumnDefault' provides' a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ColumnDefault
  = ColumnDefault RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Given a 'ValueExpression', use that as a 'ColumnDefault'. This is the preferred path to creating a
   column default. Note that it is up to the caller to ensure the 'ValueExpression' makes sense for
   the resulting 'ColumnDefinition' this will be a part of.

@since 1.0.0.0
-}
columnDefault ::
  ValueExpression ->
  ColumnDefault
columnDefault defaultValue =
  ColumnDefault (RawSql.fromString "DEFAULT " <> RawSql.toRawSql defaultValue)
