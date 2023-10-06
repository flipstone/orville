{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.ConstraintName
  ( ConstraintName
  , constraintName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL constraint name. 'ConstraintName' values constructed
via the 'constraintName' function will be properly escaped as part of the
generated SQL. E.G.

> "some_constraint_name"

'ConstraintName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

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

{- |
Construct a 'ConstraintName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
constraintName :: String -> ConstraintName
constraintName = ConstraintName . identifier
