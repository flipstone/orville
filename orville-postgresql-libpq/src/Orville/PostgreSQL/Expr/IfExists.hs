{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.IfExists
  ( IfExists
  , ifExists
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "IF EXISTS" expression

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a some other sql and use it in a place that
expected an 'IfExists', that could be done as

 > RawSql.unsafeSqlExpression "<some other than IF EXISTS>"

@since 0.10.0.0
-}
newtype IfExists
  = IfExists RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

ifExists :: IfExists
ifExists =
  IfExists $ RawSql.fromString "IF EXISTS"
