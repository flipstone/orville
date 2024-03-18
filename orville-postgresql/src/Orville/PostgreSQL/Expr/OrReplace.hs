{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.OrReplace
  ( OrReplace
  , orReplace
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "OR REPLACE" expression. E.G.

> OR REPLACE

'OrReplace' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype OrReplace
  = OrReplace RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
A value of the SQL "OR REPLACE".

@since 1.1.0.0
-}
orReplace :: OrReplace
orReplace =
  OrReplace $ RawSql.fromString "OR REPLACE"
