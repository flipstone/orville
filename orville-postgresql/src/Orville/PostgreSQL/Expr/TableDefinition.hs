{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.TableDefinition
  ( CreateTableExpr
  , createTableExpr
  , PrimaryKeyExpr
  , primaryKeyExpr
  , AlterTableExpr
  , alterTableExpr
  , AlterTableAction
  , addColumn
  , dropColumn
  , addConstraint
  , dropConstraint
  , alterColumnType
  , alterColumnSetDefault
  , alterColumnDropDefault
  , UsingClause
  , usingCast
  , alterColumnNullability
  , AlterNotNull
  , setNotNull
  , dropNotNull
  , DropTableExpr
  , dropTableExpr
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes, maybeToList)

import Orville.PostgreSQL.Expr.ColumnDefinition (ColumnDefinition)
import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (ColumnName, ConstraintName, Qualified, TableName)
import Orville.PostgreSQL.Expr.TableConstraint (TableConstraint)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a @CREATE TABLE@ statement. E.G.

> CREATE TABLE foo (id integer)

'CreateTableExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype CreateTableExpr
  = CreateTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'CreateTableExpr' with the given options.

  @since 1.0.0.0
-}
createTableExpr ::
  -- | The name to be use for the table
  Qualified TableName ->
  -- | The columns to include in the table
  [ColumnDefinition] ->
  -- | A primary key expression for the table
  Maybe PrimaryKeyExpr ->
  -- | Any table constraints to include with the table
  [TableConstraint] ->
  CreateTableExpr
createTableExpr tableName columnDefs mbPrimaryKey constraints =
  let
    columnDefsSql =
      map RawSql.toRawSql columnDefs

    constraintsSql =
      map RawSql.toRawSql constraints

    tableElementsSql =
      case mbPrimaryKey of
        Nothing ->
          columnDefsSql <> constraintsSql
        Just primaryKey ->
          RawSql.toRawSql primaryKey : (columnDefsSql <> constraintsSql)
  in
    CreateTableExpr $
      mconcat
        [ RawSql.fromString "CREATE TABLE "
        , RawSql.toRawSql tableName
        , RawSql.space
        , RawSql.leftParen
        , RawSql.intercalate RawSql.comma tableElementsSql
        , RawSql.rightParen
        ]

{- |
Type to represent a the primary key of a table. E.G.

> PRIMARY KEY (id)

'PrimaryKeyExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype PrimaryKeyExpr
  = PrimaryKeyExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'PrimaryKeyExpr' with the given columns

  @since 1.0.0.0
-}
primaryKeyExpr :: NonEmpty ColumnName -> PrimaryKeyExpr
primaryKeyExpr columnNames =
  PrimaryKeyExpr $
    mconcat
      [ RawSql.fromString "PRIMARY KEY "
      , RawSql.leftParen
      , RawSql.intercalate RawSql.comma columnNames
      , RawSql.rightParen
      ]

{- |
Type to represent a @ALTER TABLE@ statement. E.G.

> ALTER TABLE foo ADD COLUMN bar integer

'AlterTableExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype AlterTableExpr
  = AlterTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'AlterTableExpr' with the given alter table actions.

  @since 1.0.0.0
-}
alterTableExpr :: Qualified TableName -> NonEmpty AlterTableAction -> AlterTableExpr
alterTableExpr tableName actions =
  AlterTableExpr $
    RawSql.fromString "ALTER TABLE "
      <> RawSql.toRawSql tableName
      <> RawSql.space
      <> RawSql.intercalate RawSql.commaSpace actions

{- |
Type to represent an action as part of an @ALTER TABLE@ statement. E.G.

> ADD COLUMN bar integer

'AlterTableAction' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype AlterTableAction
  = AlterTableAction RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'AlterTableAction' that will add the specified column to the
  table.

  @since 1.0.0.0
-}
addColumn :: ColumnDefinition -> AlterTableAction
addColumn columnDef =
  AlterTableAction $
    RawSql.fromString "ADD COLUMN " <> RawSql.toRawSql columnDef

{- |
  Constructs an 'AlterTableAction' that will drop the specified column from the
  table.

  @since 1.0.0.0
-}
dropColumn :: ColumnName -> AlterTableAction
dropColumn columnName =
  AlterTableAction $
    RawSql.fromString "DROP COLUMN " <> RawSql.toRawSql columnName

{- |
  Constructs an 'AlterTableAction' that will add the specified constraint to the
  table.

  @since 1.0.0.0
-}
addConstraint :: TableConstraint -> AlterTableAction
addConstraint constraint =
  AlterTableAction $
    RawSql.fromString "ADD " <> RawSql.toRawSql constraint

{- |
  Constructs an 'AlterTableAction' that will drop the specified constraint from the
  table.

  @since 1.0.0.0
-}
dropConstraint :: ConstraintName -> AlterTableAction
dropConstraint constraintName =
  AlterTableAction $
    RawSql.fromString "DROP CONSTRAINT " <> RawSql.toRawSql constraintName

{- |
  Constructs an 'AlterTableAction' that will alter the type of the specified
  column.

  @since 1.0.0.0
-}
alterColumnType ::
  -- | The name of the column whose type willbe altered
  ColumnName ->
  -- | The new type to use for the column
  DataType ->
  -- | An optional 'UsingClause' to indicate to the database how data from the
  -- old type should be converted to the new type.
  Maybe UsingClause ->
  AlterTableAction
alterColumnType columnName dataType maybeUsingClause =
  AlterTableAction $
    RawSql.intercalate
      RawSql.space
      ( RawSql.fromString "ALTER COLUMN"
          : RawSql.toRawSql columnName
          : RawSql.fromString "TYPE"
          : RawSql.toRawSql dataType
          : maybeToList (fmap RawSql.toRawSql maybeUsingClause)
      )

{- |
Type to represent a @USING@ clause as part of an @ALTER COLUMN@ when changing
the type of a column. E.G.

> USING id :: integer

'UsingClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype UsingClause
  = UsingClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'UsingClause' that will cast the column to the specified type.

  @since 1.0.0.0
-}
usingCast :: ColumnName -> DataType -> UsingClause
usingCast columnName dataType =
  UsingClause $
    RawSql.fromString "USING "
      <> RawSql.toRawSql columnName
      <> RawSql.doubleColon
      <> RawSql.toRawSql dataType

{- |
  Constructs an 'AlterTableAction' that will alter the nullability of the
  column.

  @since 1.0.0.0
-}
alterColumnNullability :: ColumnName -> AlterNotNull -> AlterTableAction
alterColumnNullability columnName alterNotNull =
  AlterTableAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.toRawSql alterNotNull
      ]

{- |
Type to represent an action to alter the nullability of a column. E.G.

> SET NOT NULL

'AlterNotNull' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype AlterNotNull
  = AlterNotNull RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Sets the column to not null via @SET NOT NULL@

  @since 1.0.0.0
-}
setNotNull :: AlterNotNull
setNotNull =
  AlterNotNull $ RawSql.fromString "SET NOT NULL"

{- |
  Sets the column to allow null via @DROP NOT NULL@

  @since 1.0.0.0
-}
dropNotNull :: AlterNotNull
dropNotNull =
  AlterNotNull $ RawSql.fromString "DROP NOT NULL"

{- |
  Constructs an 'AlterTableAction' that will use @DROP DEFAULT@ to drop the
  default value of the specified column.

  @since 1.0.0.0
-}
alterColumnDropDefault :: ColumnName -> AlterTableAction
alterColumnDropDefault columnName =
  AlterTableAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "DROP DEFAULT"
      ]

{- |
  Constructs an 'AlterTableAction' that will use @SET DEFAULT@ to set the
  default value of the specified column.

  @since 1.0.0.0
-}
alterColumnSetDefault ::
  RawSql.SqlExpression valueExpression =>
  ColumnName ->
  valueExpression ->
  AlterTableAction
alterColumnSetDefault columnName defaultValue =
  AlterTableAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "SET DEFAULT"
      , RawSql.toRawSql defaultValue
      ]

{- |
Type to represent a @DROP TABLE@ statement. E.G.

> DROP TABLE FOO

'DropTableExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DropTableExpr
  = DropTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'DropTableExpr' that will drop the specified table.

  @since 1.0.0.0
-}
dropTableExpr :: Maybe IfExists -> Qualified TableName -> DropTableExpr
dropTableExpr maybeIfExists tableName =
  DropTableExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just (RawSql.fromString "DROP TABLE")
          , fmap RawSql.toRawSql maybeIfExists
          , Just (RawSql.toRawSql tableName)
          ]
      )
