{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Window.WindowClause
  ( WindowClause
  , windowClause
  , NamedWindowDefinitionExpr
  , namedWindowDefinition
  , appendNamedWindowDefinitionExpr
  )
where

import Orville.PostgreSQL.Expr.Name (WindowName)
import Orville.PostgreSQL.Expr.Window.WindowDefinitionExpr (WindowDefinitionExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a named SQL window definition. This should only be used in a @WINDOW@ clause.

'NamedWindowDefinitionExpr' provides a 'RawSql.SqlExpression' instance.
See 'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype NamedWindowDefinitionExpr = NamedWindowDefinitionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
@since 1.1.0.0
-}
instance Semigroup NamedWindowDefinitionExpr where
  (<>) = appendNamedWindowDefinitionExpr

{- | Combines two 'NamedWindowDefinitionExpr's with a comma between them.

@since 1.1.0.0
-}
appendNamedWindowDefinitionExpr :: NamedWindowDefinitionExpr -> NamedWindowDefinitionExpr -> NamedWindowDefinitionExpr
appendNamedWindowDefinitionExpr (NamedWindowDefinitionExpr a) (NamedWindowDefinitionExpr b) =
  NamedWindowDefinitionExpr (a <> RawSql.commaSpace <> b)

{- |
Builds a 'NamedWindowDefinitionExpr' with the given name and 'WindowDefinitionExpr'.
1
@since 1.1.0.0
-}
namedWindowDefinition :: WindowName -> WindowDefinitionExpr -> NamedWindowDefinitionExpr
namedWindowDefinition windowName expr =
  NamedWindowDefinitionExpr $
    RawSql.toRawSql windowName
      <> RawSql.space
      <> RawSql.fromString "AS"
      <> RawSql.space
      <> RawSql.parenthesized (RawSql.toRawSql expr)

{- |
Type to represent a SQL window clause. E.G.

> WINDOW foo , bar

'WindowClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype WindowClause
  = WindowClause RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
Builds a full 'WindowClause' with the given windowing described in the 'NamedWindowDefinitionExpr'
1
@since 1.1.0.0
-}
windowClause :: NamedWindowDefinitionExpr -> WindowClause
windowClause namedWindowDefinitionExpr =
  WindowClause $
    RawSql.fromString "WINDOW"
      <> RawSql.space
      <> RawSql.toRawSql namedWindowDefinitionExpr
