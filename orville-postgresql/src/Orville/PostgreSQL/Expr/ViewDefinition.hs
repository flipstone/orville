{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.ViewDefinition
  ( CreateViewExpr
  , createViewExpr
  , CheckOptionLevel (CascadedCheckOption, LocalCheckOption)
  , CheckOption
  , checkOption
  , viewOptionCheckOption
  , viewOptionSecurityBarrier
  , viewOptionSecurityInvoker
  , viewOptionNameCheckOption
  , viewOptionNameSecurityBarrier
  , viewOptionNameSecurityInvoker
  , DropViewExpr
  , dropViewExpr
  , AlterViewExpr
  , alterViewExpr
  , renameViewExpr
  , renameViewColumnExpr
  , setViewSchemaExpr
  , AlterViewAction
  , alterViewColumnSetDefault
  , alterViewColumnDropDefault
  , alterViewOwnerTo
  , alterViewSetOptions
  , alterViewResetOptions
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.DropBehavior (DropBehavior)
import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (ColumnName, QualifiedOrUnqualified, SchemaName, TableName)
import Orville.PostgreSQL.Expr.OrReplace (OrReplace)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import Orville.PostgreSQL.Expr.Recursive (Recursive)
import Orville.PostgreSQL.Expr.RelationOption (RelationOption, RelationOptionName, relationOption, relationOptionBool)
import Orville.PostgreSQL.Expr.RoleSpecification (RoleSpecification)
import Orville.PostgreSQL.Expr.Temporary (Temporary)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{-
   From https://www.postgresql.org/docs/15/sql-createview.html

   @@
   CREATE [ OR REPLACE ] [ TEMP | TEMPORARY ] [ RECURSIVE ] VIEW name [ ( column_name [, ...] ) ]
    [ WITH ( view_option_name [= view_option_value] [, ... ] ) ]
    AS query
    [ WITH [ CASCADED | LOCAL ] CHECK OPTION ]
   @@
-}

{- | Type to represent a @CREATE VIEW@ statement. E.G.

> CREATE VIEW foo AS SELECT id FROM bar

'CreateViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype CreateViewExpr
  = CreateViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'CreateViewExpr' that will create a view defined by the given
  query. A view is referred to by a 'TableName' because it is selected from
  in the same way as a table.

  @since 1.2.0.0
-}
createViewExpr ::
  Maybe OrReplace ->
  Maybe Temporary ->
  Maybe Recursive ->
  QualifiedOrUnqualified TableName ->
  Maybe (NonEmpty ColumnName) ->
  [RelationOption] ->
  QueryExpr ->
  Maybe CheckOption ->
  CreateViewExpr
createViewExpr maybeOrReplace maybeTemporary maybeRecursive viewName maybeColumns options query maybeCheckOption =
  CreateViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "CREATE")
      , fmap RawSql.toRawSql maybeOrReplace
      , fmap RawSql.toRawSql maybeTemporary
      , fmap RawSql.toRawSql maybeRecursive
      , Just (RawSql.fromString "VIEW")
      , Just (RawSql.toRawSql viewName)
      , fmap columnNameList maybeColumns
      , withRelationOptions options
      , Just (RawSql.fromString "AS")
      , Just (RawSql.toRawSql query)
      , fmap RawSql.toRawSql maybeCheckOption
      ]

{- | The scope of a @CHECK OPTION@ on a view: whether the conditions of the
  views that this view is defined on are checked as well.

@since 1.2.0.0
-}
data CheckOptionLevel
  = {- | @CASCADED@: the conditions of underlying views are checked too. This
    is what PostgreSQL does when no level is given.
    -}
    CascadedCheckOption
  | -- | @LOCAL@: only this view's own conditions are checked.
    LocalCheckOption
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    , -- | @since 1.2.0.0
      Show
    )

{- | Type to represent the @CHECK OPTION@ clause of a @CREATE VIEW@ statement,
  which makes inserts and updates through the view fail when the new row
  would not be visible through it. E.G.

> WITH CASCADED CHECK OPTION

'CheckOption' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype CheckOption
  = CheckOption RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'CheckOption', with the given level or with none, which
  renders as @WITH CHECK OPTION@.

@since 1.2.0.0
-}
checkOption :: Maybe CheckOptionLevel -> CheckOption
checkOption maybeLevel =
  CheckOption
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "WITH")
      , fmap (RawSql.fromString . checkOptionLevelKeyword) maybeLevel
      , Just (RawSql.fromString "CHECK OPTION")
      ]

checkOptionLevelKeyword :: CheckOptionLevel -> String
checkOptionLevelKeyword level =
  case level of
    CascadedCheckOption -> "CASCADED"
    LocalCheckOption -> "LOCAL"

{- | The @check_option@ view option, which has the same effect as the
  'CheckOption' clause. E.G.

> check_option = local

@since 1.2.0.0
-}
viewOptionCheckOption :: CheckOptionLevel -> RelationOption
viewOptionCheckOption level =
  let
    value =
      case level of
        CascadedCheckOption -> "cascaded"
        LocalCheckOption -> "local"
  in
    relationOption viewOptionNameCheckOption (Just (RawSql.unsafeSqlExpression value))

{- | The @security_barrier@ view option, which stops conditions of queries on
  the view from being evaluated before the view's own conditions. E.G.

> security_barrier = true

@since 1.2.0.0
-}
viewOptionSecurityBarrier :: Bool -> RelationOption
viewOptionSecurityBarrier flag =
  relationOption viewOptionNameSecurityBarrier (Just (relationOptionBool flag))

{- | The @security_invoker@ view option, which checks the permissions on the
  underlying relations as the user of the view rather than its owner. It
  requires PostgreSQL 15 or later. E.G.

> security_invoker = true

@since 1.2.0.0
-}
viewOptionSecurityInvoker :: Bool -> RelationOption
viewOptionSecurityInvoker flag =
  relationOption viewOptionNameSecurityInvoker (Just (relationOptionBool flag))

{- | The name of the @check_option@ view option, for use with
  'alterViewResetOptions'.

@since 1.2.0.0
-}
viewOptionNameCheckOption :: RelationOptionName
viewOptionNameCheckOption =
  RawSql.unsafeSqlExpression "check_option"

{- | The name of the @security_barrier@ view option, for use with
  'alterViewResetOptions'.

@since 1.2.0.0
-}
viewOptionNameSecurityBarrier :: RelationOptionName
viewOptionNameSecurityBarrier =
  RawSql.unsafeSqlExpression "security_barrier"

{- | The name of the @security_invoker@ view option, for use with
  'alterViewResetOptions'.

@since 1.2.0.0
-}
viewOptionNameSecurityInvoker :: RelationOptionName
viewOptionNameSecurityInvoker =
  RawSql.unsafeSqlExpression "security_invoker"

{-
   From https://www.postgresql.org/docs/15/sql-dropview.html

   @@
   DROP VIEW [ IF EXISTS ] name [, ...] [ CASCADE | RESTRICT ]
   @@
-}

{- | Type to represent a @DROP VIEW@ statement. E.G.

> DROP VIEW foo

'DropViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype DropViewExpr
  = DropViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'DropViewExpr' that will drop the views with the given names.

  @since 1.2.0.0
-}
dropViewExpr ::
  Maybe IfExists ->
  NonEmpty (QualifiedOrUnqualified TableName) ->
  Maybe DropBehavior ->
  DropViewExpr
dropViewExpr maybeIfExists viewNames maybeDropBehavior =
  DropViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "DROP VIEW")
      , fmap RawSql.toRawSql maybeIfExists
      , Just (RawSql.intercalate RawSql.commaSpace viewNames)
      , fmap RawSql.toRawSql maybeDropBehavior
      ]

{-
   From https://www.postgresql.org/docs/15/sql-alterview.html

   @@
   ALTER VIEW [ IF EXISTS ] name ALTER [ COLUMN ] column_name SET DEFAULT expression
   ALTER VIEW [ IF EXISTS ] name ALTER [ COLUMN ] column_name DROP DEFAULT
   ALTER VIEW [ IF EXISTS ] name OWNER TO { new_owner | CURRENT_ROLE | CURRENT_USER | SESSION_USER }
   ALTER VIEW [ IF EXISTS ] name RENAME [ COLUMN ] column_name TO new_column_name
   ALTER VIEW [ IF EXISTS ] name RENAME TO new_name
   ALTER VIEW [ IF EXISTS ] name SET SCHEMA new_schema
   ALTER VIEW [ IF EXISTS ] name SET ( view_option_name [= view_option_value] [, ... ] )
   ALTER VIEW [ IF EXISTS ] name RESET ( view_option_name [, ... ] )
   @@
-}

{- | Type to represent an @ALTER VIEW@ statement. E.G.

> ALTER VIEW foo RENAME TO bar

'AlterViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AlterViewExpr
  = AlterViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs an 'AlterViewExpr' that applies the given action to the view.
  An @ALTER VIEW@ statement takes exactly one action.

  @since 1.2.0.0
-}
alterViewExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  AlterViewAction ->
  AlterViewExpr
alterViewExpr maybeIfExists viewName action =
  AlterViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "ALTER VIEW")
      , fmap RawSql.toRawSql maybeIfExists
      , Just (RawSql.toRawSql viewName)
      , Just (RawSql.toRawSql action)
      ]

{- | Constructs an 'AlterViewExpr' that renames the view. The new name is
  unqualified because a view cannot be moved to another schema by renaming
  it; see 'setViewSchemaExpr'.

  @since 1.2.0.0
-}
renameViewExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  TableName ->
  AlterViewExpr
renameViewExpr maybeIfExists viewName newName =
  alterViewExpr maybeIfExists viewName . AlterViewAction $
    RawSql.fromString "RENAME TO " <> RawSql.toRawSql newName

{- | Constructs an 'AlterViewExpr' that renames a column of the view.

  @since 1.2.0.0
-}
renameViewColumnExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  -- | The current name of the column.
  ColumnName ->
  -- | The new name of the column.
  ColumnName ->
  AlterViewExpr
renameViewColumnExpr maybeIfExists viewName columnName newColumnName =
  alterViewExpr maybeIfExists viewName . AlterViewAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "RENAME COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "TO"
      , RawSql.toRawSql newColumnName
      ]

{- | Constructs an 'AlterViewExpr' that moves the view to another schema.

  @since 1.2.0.0
-}
setViewSchemaExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  SchemaName ->
  AlterViewExpr
setViewSchemaExpr maybeIfExists viewName schemaName =
  alterViewExpr maybeIfExists viewName . AlterViewAction $
    RawSql.fromString "SET SCHEMA " <> RawSql.toRawSql schemaName

{- | Type to represent an action as part of an @ALTER VIEW@ statement. E.G.

> OWNER TO some_role

'AlterViewAction' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AlterViewAction
  = AlterViewAction RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs an 'AlterViewAction' that sets the default value of a column
  of the view, which is used when rows are inserted through the view without
  a value for the column.

  @since 1.2.0.0
-}
alterViewColumnSetDefault ::
  RawSql.SqlExpression valueExpression =>
  ColumnName ->
  valueExpression ->
  AlterViewAction
alterViewColumnSetDefault columnName defaultValue =
  AlterViewAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "SET DEFAULT"
      , RawSql.toRawSql defaultValue
      ]

{- | Constructs an 'AlterViewAction' that removes the default value of a
  column of the view.

  @since 1.2.0.0
-}
alterViewColumnDropDefault :: ColumnName -> AlterViewAction
alterViewColumnDropDefault columnName =
  AlterViewAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "DROP DEFAULT"
      ]

{- | Constructs an 'AlterViewAction' that changes the owner of the view.

  @since 1.2.0.0
-}
alterViewOwnerTo :: RoleSpecification -> AlterViewAction
alterViewOwnerTo role =
  AlterViewAction $
    RawSql.fromString "OWNER TO " <> RawSql.toRawSql role

{- | Constructs an 'AlterViewAction' that sets the given view options.

  @since 1.2.0.0
-}
alterViewSetOptions :: NonEmpty RelationOption -> AlterViewAction
alterViewSetOptions options =
  AlterViewAction $
    RawSql.fromString "SET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace options)

{- | Constructs an 'AlterViewAction' that resets the named view options to
  their defaults.

  @since 1.2.0.0
-}
alterViewResetOptions :: NonEmpty RelationOptionName -> AlterViewAction
alterViewResetOptions optionNames =
  AlterViewAction $
    RawSql.fromString "RESET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace optionNames)

withRelationOptions :: [RelationOption] -> Maybe RawSql.RawSql
withRelationOptions options =
  case options of
    [] -> Nothing
    _ -> Just (RawSql.fromString "WITH " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace options))

columnNameList :: NonEmpty ColumnName -> RawSql.RawSql
columnNameList =
  RawSql.parenthesized . RawSql.intercalate RawSql.comma
