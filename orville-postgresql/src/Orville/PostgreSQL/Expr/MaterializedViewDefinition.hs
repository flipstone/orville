{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.MaterializedViewDefinition
  ( CreateMaterializedViewExpr
  , createMaterializedViewExpr
  , WithDataExpr
  , withData
  , withNoData
  , DropMaterializedViewExpr
  , dropMaterializedViewExpr
  , RefreshMaterializedViewExpr
  , refreshMaterializedViewExpr
  , AlterMaterializedViewExpr
  , alterMaterializedViewExpr
  , renameMaterializedViewExpr
  , renameMaterializedViewColumnExpr
  , setMaterializedViewSchemaExpr
  , alterMaterializedViewExtensionDependencyExpr
  , alterAllMaterializedViewsInTablespaceExpr
  , AlterMaterializedViewAction
  , alterMaterializedViewColumnSetStatistics
  , alterMaterializedViewColumnSetOptions
  , alterMaterializedViewColumnResetOptions
  , alterMaterializedViewColumnSetStorage
  , alterMaterializedViewColumnSetCompression
  , alterMaterializedViewClusterOn
  , alterMaterializedViewSetWithoutCluster
  , alterMaterializedViewSetAccessMethod
  , alterMaterializedViewSetTablespace
  , alterMaterializedViewSetOptions
  , alterMaterializedViewResetOptions
  , alterMaterializedViewOwnerTo
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.ColumnStorage (ColumnStorage, CompressionMethod)
import Orville.PostgreSQL.Expr.DropBehavior (DropBehavior)
import Orville.PostgreSQL.Expr.ExtensionDependency (ExtensionDependency)
import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.IfNotExists (IfNotExists)
import Orville.PostgreSQL.Expr.Index (ConcurrentlyExpr)
import Orville.PostgreSQL.Expr.Name (AccessMethodName, ColumnName, IndexName, QualifiedOrUnqualified, SchemaName, TableName, TablespaceName)
import Orville.PostgreSQL.Expr.NoWait (NoWait)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import Orville.PostgreSQL.Expr.RelationOption (RelationOption, RelationOptionName)
import Orville.PostgreSQL.Expr.RoleSpecification (RoleSpecification)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a @CREATE MATERIALIZED VIEW@ statement. E.G.

> CREATE MATERIALIZED VIEW foo AS SELECT id FROM bar WITH DATA

'CreateMaterializedViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype CreateMaterializedViewExpr
  = CreateMaterializedViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'CreateMaterializedViewExpr' that will create a materialized
  view defined by the given query. A materialized view is referred to by a
  'TableName' because it is selected from in the same way as a table.

  @since 1.2.0.0
-}
createMaterializedViewExpr ::
  Maybe IfNotExists ->
  QualifiedOrUnqualified TableName ->
  Maybe (NonEmpty ColumnName) ->
  Maybe AccessMethodName ->
  [RelationOption] ->
  Maybe TablespaceName ->
  QueryExpr ->
  Maybe WithDataExpr ->
  CreateMaterializedViewExpr
createMaterializedViewExpr maybeIfNotExists viewName maybeColumns maybeAccessMethod options maybeTablespace query maybeWithData =
  CreateMaterializedViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "CREATE MATERIALIZED VIEW")
      , fmap RawSql.toRawSql maybeIfNotExists
      , Just (RawSql.toRawSql viewName)
      , fmap columnNameList maybeColumns
      , fmap ((RawSql.fromString "USING " <>) . RawSql.toRawSql) maybeAccessMethod
      , withRelationOptions options
      , fmap tablespaceClause maybeTablespace
      , Just (RawSql.fromString "AS")
      , Just (RawSql.toRawSql query)
      , fmap RawSql.toRawSql maybeWithData
      ]

{- | Type to represent the @WITH DATA@ or @WITH NO DATA@ clause of a
  @CREATE MATERIALIZED VIEW@ or @REFRESH MATERIALIZED VIEW@ statement. E.G.

> WITH NO DATA

'WithDataExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype WithDataExpr
  = WithDataExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | A @WITH DATA@ clause, which populates the materialized view from its
  query.

  @since 1.2.0.0
-}
withData :: WithDataExpr
withData =
  WithDataExpr (RawSql.fromString "WITH DATA")

{- | A @WITH NO DATA@ clause, which leaves the materialized view unpopulated and unreadable until it
is refreshed.

@since 1.2.0.0
-}
withNoData :: WithDataExpr
withNoData =
  WithDataExpr (RawSql.fromString "WITH NO DATA")

{- | Type to represent a @DROP MATERIALIZED VIEW@ statement. E.G.

> DROP MATERIALIZED VIEW foo

'DropMaterializedViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype DropMaterializedViewExpr
  = DropMaterializedViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'DropMaterializedViewExpr' that will drop the materialized views with the given
names.

@since 1.2.0.0
-}
dropMaterializedViewExpr ::
  Maybe IfExists ->
  NonEmpty (QualifiedOrUnqualified TableName) ->
  Maybe DropBehavior ->
  DropMaterializedViewExpr
dropMaterializedViewExpr maybeIfExists viewNames maybeDropBehavior =
  DropMaterializedViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "DROP MATERIALIZED VIEW")
      , fmap RawSql.toRawSql maybeIfExists
      , Just (RawSql.intercalate RawSql.commaSpace viewNames)
      , fmap RawSql.toRawSql maybeDropBehavior
      ]

{- | Type to represent a @REFRESH MATERIALIZED VIEW@ statement. E.G.

> REFRESH MATERIALIZED VIEW CONCURRENTLY foo WITH DATA

'RefreshMaterializedViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype RefreshMaterializedViewExpr
  = RefreshMaterializedViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'RefreshMaterializedViewExpr' that will replace the stored contents of the
  materialized view with the current result of its query.

Note that using @CONCURRENTLY@ allows the materialized view be read while it is
refreshed. PostgreSQL requires a unique index on the materialized view for this, and does not allow
it together with @WITH NO DATA@. It is up to the caller to ensure this.

@since 1.2.0.0
-}
refreshMaterializedViewExpr ::
  Maybe ConcurrentlyExpr ->
  QualifiedOrUnqualified TableName ->
  Maybe WithDataExpr ->
  RefreshMaterializedViewExpr
refreshMaterializedViewExpr maybeConcurrently viewName maybeWithData =
  RefreshMaterializedViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "REFRESH MATERIALIZED VIEW")
      , fmap RawSql.toRawSql maybeConcurrently
      , Just (RawSql.toRawSql viewName)
      , fmap RawSql.toRawSql maybeWithData
      ]

{- | Type to represent an @ALTER MATERIALIZED VIEW@ statement. E.G.

> ALTER MATERIALIZED VIEW foo RENAME TO bar

'AlterMaterializedViewExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AlterMaterializedViewExpr
  = AlterMaterializedViewExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs an 'AlterMaterializedViewExpr' that applies the given actions
  to the materialized view.

  @since 1.2.0.0
-}
alterMaterializedViewExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  NonEmpty AlterMaterializedViewAction ->
  AlterMaterializedViewExpr
alterMaterializedViewExpr maybeIfExists viewName actions =
  AlterMaterializedViewExpr
    . RawSql.intercalate RawSql.space
    . catMaybes
    $ [ Just (RawSql.fromString "ALTER MATERIALIZED VIEW")
      , fmap RawSql.toRawSql maybeIfExists
      , Just (RawSql.toRawSql viewName)
      , Just (RawSql.intercalate RawSql.commaSpace actions)
      ]

{- | Constructs an 'AlterMaterializedViewExpr' that renames the materialized view. The new name is
unqualified because a materialized view cannot be moved to another schema by renaming it. See
'setMaterializedViewSchemaExpr'.

@since 1.2.0.0
-}
renameMaterializedViewExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  TableName ->
  AlterMaterializedViewExpr
renameMaterializedViewExpr maybeIfExists viewName newName =
  alterMaterializedViewExpr maybeIfExists viewName . pure . AlterMaterializedViewAction $
    RawSql.fromString "RENAME TO " <> RawSql.toRawSql newName

{- | Constructs an 'AlterMaterializedViewExpr' that renames a column of the materialized view.

@since 1.2.0.0
-}
renameMaterializedViewColumnExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  -- | The current name of the column.
  ColumnName ->
  -- | The new name of the column.
  ColumnName ->
  AlterMaterializedViewExpr
renameMaterializedViewColumnExpr maybeIfExists viewName columnName newColumnName =
  alterMaterializedViewExpr maybeIfExists viewName . pure . AlterMaterializedViewAction $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "RENAME COLUMN"
      , RawSql.toRawSql columnName
      , RawSql.fromString "TO"
      , RawSql.toRawSql newColumnName
      ]

{- | Constructs an 'AlterMaterializedViewExpr' that moves the materialized view to another schema.

@since 1.2.0.0
-}
setMaterializedViewSchemaExpr ::
  Maybe IfExists ->
  QualifiedOrUnqualified TableName ->
  SchemaName ->
  AlterMaterializedViewExpr
setMaterializedViewSchemaExpr maybeIfExists viewName schemaName =
  alterMaterializedViewExpr maybeIfExists viewName . pure . AlterMaterializedViewAction $
    RawSql.fromString "SET SCHEMA " <> RawSql.toRawSql schemaName

{- | Constructs an 'AlterMaterializedViewExpr' that adds or removes the materialized view's dependency
on an extension. This form of the statement has no @IF EXISTS@.

@since 1.2.0.0
-}
alterMaterializedViewExtensionDependencyExpr ::
  QualifiedOrUnqualified TableName ->
  ExtensionDependency ->
  AlterMaterializedViewExpr
alterMaterializedViewExtensionDependencyExpr viewName dependency =
  AlterMaterializedViewExpr $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "ALTER MATERIALIZED VIEW"
      , RawSql.toRawSql viewName
      , RawSql.toRawSql dependency
      ]

{- | Constructs an 'AlterMaterializedViewExpr' that moves all the materialized
views in a tablespace to another tablespace.

@since 1.2.0.0
-}
alterAllMaterializedViewsInTablespaceExpr ::
  TablespaceName ->
  [RoleSpecification] ->
  TablespaceName ->
  Maybe NoWait ->
  AlterMaterializedViewExpr
alterAllMaterializedViewsInTablespaceExpr fromTablespace owners toTablespace maybeNoWait =
  let
    ownedBy =
      case owners of
        [] -> Nothing
        _ -> Just (RawSql.fromString "OWNED BY " <> RawSql.intercalate RawSql.commaSpace owners)
  in
    AlterMaterializedViewExpr
      . RawSql.intercalate RawSql.space
      . catMaybes
      $ [ Just (RawSql.fromString "ALTER MATERIALIZED VIEW ALL IN")
        , Just (tablespaceClause fromTablespace)
        , ownedBy
        , Just (RawSql.fromString "SET")
        , Just (tablespaceClause toTablespace)
        , fmap RawSql.toRawSql maybeNoWait
        ]

{- | Type to represent an action as part of an @ALTER MATERIALIZED VIEW@
  statement. E.G.

> SET WITHOUT CLUSTER

'AlterMaterializedViewAction' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AlterMaterializedViewAction
  = AlterMaterializedViewAction RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs an 'AlterMaterializedViewAction' that sets the statistics target of a column, for use
by the planner. A target of @-1@ reverts to the server's default.

@since 1.2.0.0
-}
alterMaterializedViewColumnSetStatistics ::
  ColumnName ->
  Int ->
  AlterMaterializedViewAction
alterMaterializedViewColumnSetStatistics columnName target =
  alterColumnAction columnName $
    RawSql.fromString "SET STATISTICS " <> RawSql.intDecLiteral target

{- | Constructs an 'AlterMaterializedViewAction' that sets attribute options of a column, such as
@n_distinct@.

@since 1.2.0.0
-}
alterMaterializedViewColumnSetOptions ::
  ColumnName ->
  NonEmpty RelationOption ->
  AlterMaterializedViewAction
alterMaterializedViewColumnSetOptions columnName options =
  alterColumnAction columnName $
    RawSql.fromString "SET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace options)

{- | Constructs an 'AlterMaterializedViewAction' that resets attribute options of a column to their
defaults.

@since 1.2.0.0
-}
alterMaterializedViewColumnResetOptions ::
  ColumnName ->
  NonEmpty RelationOptionName ->
  AlterMaterializedViewAction
alterMaterializedViewColumnResetOptions columnName optionNames =
  alterColumnAction columnName $
    RawSql.fromString "RESET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace optionNames)

{- | Constructs an 'AlterMaterializedViewAction' that sets the storage mode of a column.

@since 1.2.0.0
-}
alterMaterializedViewColumnSetStorage ::
  ColumnName ->
  ColumnStorage ->
  AlterMaterializedViewAction
alterMaterializedViewColumnSetStorage columnName storage =
  alterColumnAction columnName $
    RawSql.fromString "SET STORAGE " <> RawSql.toRawSql storage

{- | Constructs an 'AlterMaterializedViewAction' that sets the compression method of a column.

@since 1.2.0.0
-}
alterMaterializedViewColumnSetCompression ::
  ColumnName ->
  CompressionMethod ->
  AlterMaterializedViewAction
alterMaterializedViewColumnSetCompression columnName method =
  alterColumnAction columnName $
    RawSql.fromString "SET COMPRESSION " <> RawSql.toRawSql method

{- | Constructs an 'AlterMaterializedViewAction' that marks the named index as the one to cluster the
materialized view on.

@since 1.2.0.0
-}
alterMaterializedViewClusterOn :: IndexName -> AlterMaterializedViewAction
alterMaterializedViewClusterOn indexName =
  AlterMaterializedViewAction $
    RawSql.fromString "CLUSTER ON " <> RawSql.toRawSql indexName

{- | Constructs an 'AlterMaterializedViewAction' that removes the clustering index of the materialized
view.

@since 1.2.0.0
-}
alterMaterializedViewSetWithoutCluster :: AlterMaterializedViewAction
alterMaterializedViewSetWithoutCluster =
  AlterMaterializedViewAction (RawSql.fromString "SET WITHOUT CLUSTER")

{- | Constructs an 'AlterMaterializedViewAction' that changes the table access
method of the materialized view.

@since 1.2.0.0
-}
alterMaterializedViewSetAccessMethod :: AccessMethodName -> AlterMaterializedViewAction
alterMaterializedViewSetAccessMethod method =
  AlterMaterializedViewAction $
    RawSql.fromString "SET ACCESS METHOD " <> RawSql.toRawSql method

{- | Constructs an 'AlterMaterializedViewAction' that moves the materialized view to another
tablespace.

@since 1.2.0.0
-}
alterMaterializedViewSetTablespace :: TablespaceName -> AlterMaterializedViewAction
alterMaterializedViewSetTablespace tablespace =
  AlterMaterializedViewAction $
    RawSql.fromString "SET " <> tablespaceClause tablespace

{- | Constructs an 'AlterMaterializedViewAction' that sets storage parameters of the materialized
view.

@since 1.2.0.0
-}
alterMaterializedViewSetOptions :: NonEmpty RelationOption -> AlterMaterializedViewAction
alterMaterializedViewSetOptions options =
  AlterMaterializedViewAction $
    RawSql.fromString "SET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace options)

{- | Constructs an 'AlterMaterializedViewAction' that resets storage parameters of the materialized
view to their defaults.

@since 1.2.0.0
-}
alterMaterializedViewResetOptions :: NonEmpty RelationOptionName -> AlterMaterializedViewAction
alterMaterializedViewResetOptions optionNames =
  AlterMaterializedViewAction $
    RawSql.fromString "RESET " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace optionNames)

{- | Constructs an 'AlterMaterializedViewAction' that changes the owner of the materialized view.

@since 1.2.0.0
-}
alterMaterializedViewOwnerTo :: RoleSpecification -> AlterMaterializedViewAction
alterMaterializedViewOwnerTo role =
  AlterMaterializedViewAction $
    RawSql.fromString "OWNER TO " <> RawSql.toRawSql role

alterColumnAction :: ColumnName -> RawSql.RawSql -> AlterMaterializedViewAction
alterColumnAction columnName action =
  AlterMaterializedViewAction $
    RawSql.fromString "ALTER COLUMN "
      <> RawSql.toRawSql columnName
      <> RawSql.space
      <> action

tablespaceClause :: TablespaceName -> RawSql.RawSql
tablespaceClause tablespace =
  RawSql.fromString "TABLESPACE " <> RawSql.toRawSql tablespace

withRelationOptions :: [RelationOption] -> Maybe RawSql.RawSql
withRelationOptions options =
  case options of
    [] -> Nothing
    _ -> Just (RawSql.fromString "WITH " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace options))

columnNameList :: NonEmpty ColumnName -> RawSql.RawSql
columnNameList =
  RawSql.parenthesized . RawSql.intercalate RawSql.comma
