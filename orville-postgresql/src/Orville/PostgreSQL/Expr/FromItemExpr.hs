{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.FromItemExpr
  ( FromItemExpr
  , tableFromItem
  , tableFromItemWithAlias
  )
where

import Orville.PostgreSQL.Expr.Name (AliasExpr, QualifiedOrUnqualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the item in the @FROM@ clause of a @SELECT
statement. E.G. just the

> foo

in

> FROM foo

'FromItemExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FromItemExpr
  = FromItemExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

-- | @since 1.1.0.0
instance Semigroup FromItemExpr where
  FromItemExpr a <> FromItemExpr b =
    FromItemExpr $ RawSql.appendWithCommaSpace a b

-- | @since 1.1.0.0
instance Monoid FromItemExpr where
  mempty = FromItemExpr mempty

{- |
  Constructs a 'FromItemExpr' consisting of just the specified table
  name.

  @since 1.1.0.0
-}
tableFromItem :: QualifiedOrUnqualified TableName -> FromItemExpr
tableFromItem =
  FromItemExpr
    . RawSql.toRawSql

{- |
  Constructs a 'FromItemExpr' consisting of the specified table AS the given alias.

  @since 1.1.0.0
-}
tableFromItemWithAlias :: AliasExpr -> QualifiedOrUnqualified TableName -> FromItemExpr
tableFromItemWithAlias alias qualifiedTableName =
  FromItemExpr $
    RawSql.intercalate
      RawSql.space
      [ RawSql.toRawSql qualifiedTableName
      , RawSql.fromString "AS"
      , RawSql.toRawSql alias
      ]
