{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.TableConstraint
  ( TableConstraint
  , uniqueConstraint
  , foreignKeyConstraint
  , ForeignKeyActionExpr
  , restrictExpr
  , cascadeExpr
  , setNullExpr
  , setDefaultExpr
  , ForeignKeyDeleteActionExpr
  , foreignKeyDeleteActionExpr
  , ForeignKeyUpdateActionExpr
  , foreignKeyUpdateActionExpr
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a table constraint that would be part of a @CREATE TABLE@ or
@ALTER TABLE@ statement. For instances, the @UNIQUE@ constraint in

> CREATE TABLE FOO
>  ( id integer
>  , UNIQUE id
>  )
>

'TableConstraint' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype TableConstraint
  = TableConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'TableConstraint' will create a @UNIQUE@ constraint on the
  given columns.

  @since 1.0.0.0
-}
uniqueConstraint :: NonEmpty ColumnName -> TableConstraint
uniqueConstraint columnNames =
  TableConstraint $
    RawSql.fromString "UNIQUE "
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen

{- |
Type to represent a foreign key action on a @FOREIGN KEY@ constraint. E.G.
the @CASCADE@ in

> FOREIGN KEY (foo_id) REFERENCES foo (id) ON DELETE CASCADE

'ForeignKeyActionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ForeignKeyActionExpr
  = ForeignKeyActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  The foreign key action @RESTRICT@.

  @since 1.0.0.0
-}
restrictExpr :: ForeignKeyActionExpr
restrictExpr = ForeignKeyActionExpr $ RawSql.fromString "RESTRICT"

{- |
  The foreign key action @CASCADE@.

  @since 1.0.0.0
-}
cascadeExpr :: ForeignKeyActionExpr
cascadeExpr = ForeignKeyActionExpr $ RawSql.fromString "CASCADE"

{- |
  The foreign key action @SET NULL@.

  @since 1.0.0.0
-}
setNullExpr :: ForeignKeyActionExpr
setNullExpr = ForeignKeyActionExpr $ RawSql.fromString "SET NULL"

{- |
  The foreign key action @SET DEFAULT@.

  @since 1.0.0.0
-}
setDefaultExpr :: ForeignKeyActionExpr
setDefaultExpr = ForeignKeyActionExpr $ RawSql.fromString "SET DEFAULT"

{- |
Type to represent an foreign key update action on a @FOREIGN KEY@ constraint. E.G.
the @ON UPDATE RESTRICT@ in

> FOREIGN KEY (foo_id) REFERENCES foo (id) ON UPDATE RESTRICT

'ForeignKeyUpdateActionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ForeignKeyUpdateActionExpr
  = ForeignKeyUpdateActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'ForeignKeyActionExpr' that use the given 'ForeignKeyActionExpr'
  in an @ON UPDATE@ clause for a foreign key.

  @since 1.0.0.0
-}
foreignKeyUpdateActionExpr :: ForeignKeyActionExpr -> ForeignKeyUpdateActionExpr
foreignKeyUpdateActionExpr action =
  ForeignKeyUpdateActionExpr $
    RawSql.fromString "ON UPDATE"
      <> RawSql.space
      <> RawSql.toRawSql action

{- |
Type to represent an foreign key update action on a @FOREIGN KEY@ constraint. E.G.
the @ON DELETE RESTRICT@ in

> FOREIGN KEY (foo_id) REFERENCES foo (id) ON DELETE RESTRICT

'ForeignKeyDeleteActionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ForeignKeyDeleteActionExpr
  = ForeignKeyDeleteActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'ForeignKeyActionExpr' that use the given 'ForeignKeyActionExpr'
  in an @ON UPDATE@ clause for a foreign key.

  @since 1.0.0.0
-}
foreignKeyDeleteActionExpr :: ForeignKeyActionExpr -> ForeignKeyDeleteActionExpr
foreignKeyDeleteActionExpr action =
  ForeignKeyDeleteActionExpr $
    RawSql.fromString "ON DELETE"
      <> RawSql.space
      <> RawSql.toRawSql action

{- |
  Constructs a 'TableConstraint' that represent a @FOREIGN KEY@ constraint

  @since 1.0.0.0
-}
foreignKeyConstraint ::
  -- | The names of the columns in the source table that form the foreign key
  NonEmpty ColumnName ->
  -- | The table of the table that the foreign key references
  Qualified TableName ->
  -- | The names of the columns in the foreign table that the foreign key references
  NonEmpty ColumnName ->
  -- | An optional @ON UPDATE@ foreign key action to perform
  Maybe ForeignKeyUpdateActionExpr ->
  -- | An optional @ON DELETE@ foreign key action to perform
  Maybe ForeignKeyDeleteActionExpr ->
  TableConstraint
foreignKeyConstraint columnNames foreignTableName foreignColumnNames mbUpdateAction mbDeleteAction =
  TableConstraint $
    RawSql.fromString "FOREIGN KEY "
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen
      <> RawSql.fromString " REFERENCES "
      <> RawSql.toRawSql foreignTableName
      <> RawSql.space
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma foreignColumnNames
      <> RawSql.rightParen
      <> maybe mempty (\updateAction -> RawSql.space <> RawSql.toRawSql updateAction) mbUpdateAction
      <> maybe mempty (\deleteAction -> RawSql.space <> RawSql.toRawSql deleteAction) mbDeleteAction
