{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.PolicyExpr.PolicyRole
  ( PolicyRole
  , policyRole
  ) where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the name of a database role that a policy applies to, as
used in the @TO@ clause of a @CREATE POLICY@ statement. 'PolicyRole' values
constructed via the 'policyRole' function will be properly escaped as part of
the generated SQL. E.G.

> "some_role"

'PolicyRole' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyRole
  = PolicyRole Identifier
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    , -- | @since 1.2.0.0
      IdentifierExpression
    )

{- | Construct a 'PolicyRole' from a 'String' with proper escaping as part of the generated SQL.

@since 1.2.0.0
-}
policyRole :: String -> PolicyRole
policyRole =
  PolicyRole . identifier
