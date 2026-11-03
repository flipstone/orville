{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.ExtensionDependency
  ( ExtensionDependency
  , dependsOnExtension
  , noDependsOnExtension
  )
where

import Orville.PostgreSQL.Expr.Name (ExtensionName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the @[NO] DEPENDS ON EXTENSION@ form of an @ALTER@
  statement, which marks an object as belonging to an extension so that it is
  dropped along with it. E.G.

> DEPENDS ON EXTENSION "some_extension"

'ExtensionDependency' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype ExtensionDependency
  = ExtensionDependency RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Marks the object as depending on the named extension.

@since 1.2.0.0
-}
dependsOnExtension :: ExtensionName -> ExtensionDependency
dependsOnExtension extension =
  ExtensionDependency $
    RawSql.fromString "DEPENDS ON EXTENSION " <> RawSql.toRawSql extension

{- | Removes the object's dependency on the named extension.

@since 1.2.0.0
-}
noDependsOnExtension :: ExtensionName -> ExtensionDependency
noDependsOnExtension extension =
  ExtensionDependency $
    RawSql.fromString "NO DEPENDS ON EXTENSION " <> RawSql.toRawSql extension
