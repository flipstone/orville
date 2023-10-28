{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.IfExists
  ( IfExists
  , ifExists
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "IF EXISTS" expression. E.G.

> IF EXISTS

'IfExists' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype IfExists
  = IfExists RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
A value of the SQL "IF EXISTS".

@since 1.0.0.0
-}
ifExists :: IfExists
ifExists =
  IfExists $ RawSql.fromString "IF EXISTS"
