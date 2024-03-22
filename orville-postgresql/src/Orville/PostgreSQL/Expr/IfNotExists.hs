{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.IfNotExists
  ( IfNotExists
  , ifNotExists
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "IF NOT EXISTS" expression. E.G.

> IF NOT EXISTS

It is notable that 'IfNotExists' and 'Orville.PostgreSQL.Expr.IfExists' are not usable in all the
same places.

'IfNotExists' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype IfNotExists
  = IfNotExists RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
A value of the SQL "IF NOT EXISTS".

@since 1.1.0.0
-}
ifNotExists :: IfNotExists
ifNotExists =
  IfNotExists $ RawSql.fromString "IF NOT EXISTS"
