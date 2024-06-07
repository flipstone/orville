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
  )
where

import qualified Data.List.NonEmpty as NEL

import Orville.PostgreSQL.Expr.Name (WindowName)
import Orville.PostgreSQL.Expr.Window.WindowDefinitionExpr (WindowDefinitionExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

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
Builds a full 'WindowClause' with the given ordering described in the 'WindowDefinitionExpr'
1
@since 1.1.0.0
-}
windowClause :: NEL.NonEmpty (WindowName, WindowDefinitionExpr) -> WindowClause
windowClause namesAndExprs =
  let
    pairToRaw :: (WindowName, WindowDefinitionExpr) -> RawSql.RawSql
    pairToRaw (windowName, expr) =
      RawSql.toRawSql windowName
        <> RawSql.space
        <> RawSql.fromString "AS"
        <> RawSql.space
        <> RawSql.parenthesized (RawSql.toRawSql expr)
  in
    WindowClause $
      RawSql.fromString "WINDOW"
        <> RawSql.space
        <> RawSql.intercalate RawSql.commaSpace (fmap pairToRaw namesAndExprs)
