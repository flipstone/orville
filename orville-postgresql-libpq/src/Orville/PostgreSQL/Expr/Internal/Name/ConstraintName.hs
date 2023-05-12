{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.ConstraintName
  ( ConstraintName,
    constraintName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL constraint name. 'ConstraintName' values constructed
via the 'constraintName' function will be properly escaped as part of the
generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) constraint name by hand and
use it in a place that expected a 'ConstraintName', that could be done as

 > RawSql.unsafeSqlExpression "my_constraint_name"

@since 0.10.0.0
-}
newtype ConstraintName
  = ConstraintName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

constraintName :: String -> ConstraintName
constraintName = ConstraintName . identifier
