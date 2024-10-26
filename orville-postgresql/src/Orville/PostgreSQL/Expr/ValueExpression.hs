{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.ValueExpression
  ( ValueExpression
  , cast
  , ParameterName
  , columnReference
  , valueExpression
  , rowValueConstructor
  , functionCall
  , functionCallNamedParams
  , aliasReference
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.Name (AliasExpr, ColumnName, FunctionName, QualifiedOrUnqualified)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Raw.SqlValue (SqlValue)

{- | Type to represent an arbitrary value in a SQL expression. This could be a
constant value, a column reference or any arbitrary calculated expression.
E.G.

> (foo + bar) > 20

'ValueExpression' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ValueExpression = ValueExpression RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Performs a SQL type cast to the specified type on the given 'ValueExpression'.
E.G.

> foo :: integer

@since 1.0.0.0
-}
cast :: ValueExpression -> DataType -> ValueExpression
cast value dataType =
  ValueExpression $
    RawSql.toRawSql value
      <> RawSql.fromString "::"
      <> RawSql.toRawSql dataType

{- | Uses a 'ColumnName' to reference a column as a 'ValueExpression'. This
is the equivalent of simply writing the column name as the expression. E.G.

> foo

@since 1.0.0.0
-}
columnReference :: QualifiedOrUnqualified ColumnName -> ValueExpression
columnReference = ValueExpression . RawSql.toRawSql

{- | Uses an 'AliasExpr' to reference an aliased expression as a 'ValueExpression'. This
is the equivalent of simply writing the alias as the expression. E.G.

> foo

@since 1.1.0.0
-}
aliasReference :: AliasExpr -> ValueExpression
aliasReference = ValueExpression . RawSql.toRawSql

{- | Uses the given 'SqlValue' as a constant expression. The value will be passed
  as a statement parameter, not as a literal expression, so there is not need
  to worry about escaping. However, there are a few places (usually in DDL)
  where PostgreSQL does not support values passed as parameters where this
  cannot be used.

  @since 1.0.0.0
-}
valueExpression :: SqlValue -> ValueExpression
valueExpression = ValueExpression . RawSql.parameter

{- | Constructs a PostgreSQL row value expression from the given list of
expressions. E.G.

> (foo, bar, now())

@since 1.0.0.0
-}
rowValueConstructor :: NE.NonEmpty ValueExpression -> ValueExpression
rowValueConstructor elements =
  ValueExpression $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma elements
      <> RawSql.rightParen

{- | Constructs a 'ValueExpression' that will call the specified PostgreSQL
function with the given arguments passed as position parameters. E.G.

> nextval(sequence_name)

@since 1.0.0.0
-}
functionCall :: FunctionName -> [ValueExpression] -> ValueExpression
functionCall functionName parameters =
  ValueExpression $
    RawSql.toRawSql functionName
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma parameters
      <> RawSql.rightParen

{- | Type to represent the name of a name parameter in a PostgreSQL function call.
E.G.

> foo

in

> some_func(foo => 1)

'ParameterName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ParameterName = ParameterName RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'ValueExpression' that will call the specified PostgreSQL
function with the given arguments passed as named parameters. E.G.

> make_interval(years => 1)

@since 1.0.0.0
-}
functionCallNamedParams :: FunctionName -> [(ParameterName, ValueExpression)] -> ValueExpression
functionCallNamedParams functionName parameters =
  ValueExpression $
    RawSql.toRawSql functionName
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap (uncurry namedParameterArgument) parameters)
      <> RawSql.rightParen

{- | Constructs a sql fragment that will pass the given named argument with the
  specified value.

  @since 1.0.0.0
-}
namedParameterArgument :: ParameterName -> ValueExpression -> RawSql.RawSql
namedParameterArgument name value =
  RawSql.toRawSql name
    <> RawSql.space
    <> RawSql.fromString "=>"
    <> RawSql.space
    <> RawSql.toRawSql value
