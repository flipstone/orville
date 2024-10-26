{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

Types and functionality related to loading PostgreSQL extensions. This does not contain features
related to any specific extension, but serves as the basis for loading some extension.

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Extension
  ( CreateExtensionExpr
  , createExtensionExpr
  , DropExtensionExpr
  , dropExtensionExpr
  , ExtensionActionExpr
  , extensionCascadeExpr
  , extensionRestrictExpr
  ) where

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.IfNotExists (IfNotExists)
import Orville.PostgreSQL.Expr.Name (ExtensionName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL "CREATE EXTENSION" statement. E.G.

> CREATE EXTENSION foo

'CreateIndexExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype CreateExtensionExpr = CreateExtensionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Construct a SQL CREATE EXTENSION statement from the extension name and if the statement should
not fail on loading an extension with the same name as an already loaded one.

@since 1.1.0.0
-}
createExtensionExpr :: ExtensionName -> Maybe IfNotExists -> Maybe ExtensionActionExpr -> CreateExtensionExpr
createExtensionExpr extension mbIfNotExists mbAction =
  CreateExtensionExpr $
    RawSql.fromString "CREATE EXTENSION "
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbIfNotExists)
      <> RawSql.toRawSql extension
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbAction)

{- | Type to represent a SQL "DROP EXTENSION" statement. E.G.

> DROP EXTENSION foo

'DropIndexExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype DropExtensionExpr = DropExtensionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Construct a SQL DROP EXTENSION statement from the extension name and if the statement should
not fail on loading an extension with the same name as an already loaded one.

@since 1.1.0.0
-}
dropExtensionExpr :: ExtensionName -> Maybe IfExists -> Maybe ExtensionActionExpr -> DropExtensionExpr
dropExtensionExpr extension mbIfExists mbAction =
  DropExtensionExpr $
    RawSql.fromString "DROP EXTENSION "
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbIfExists)
      <> RawSql.toRawSql extension
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbAction)

{- | Type to represent a extension action on a @EXTENSION@. E.G.
the @CASCADE@ in

> CREATE EXTENSION foo CASCADE

'ExtensionActionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype ExtensionActionExpr
  = ExtensionActionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The extension action @RESTRICT@.

  @since 1.1.0.0
-}
extensionRestrictExpr :: ExtensionActionExpr
extensionRestrictExpr = ExtensionActionExpr $ RawSql.fromString "RESTRICT"

{- | The extension action @CASCADE@.

  @since 1.1.0.0
-}
extensionCascadeExpr :: ExtensionActionExpr
extensionCascadeExpr = ExtensionActionExpr $ RawSql.fromString "CASCADE"
