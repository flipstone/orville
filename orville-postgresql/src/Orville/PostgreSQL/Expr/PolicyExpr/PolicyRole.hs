{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.PolicyExpr.PolicyRole
  ( PolicyRoleExpr
  , namedPolicyRole
  , publicPolicyRole
  , currentRolePolicyRole
  , currentUserPolicyRole
  , sessionUserPolicyRole
  ) where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a role target in the @TO@ clause of a @CREATE POLICY@
or @ALTER POLICY@ statement: either the name of a database role or one of
the special targets @PUBLIC@, @CURRENT_ROLE@, @CURRENT_USER@ or
@SESSION_USER@.

'PolicyRoleExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyRoleExpr
  = PolicyRoleExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Construct a 'PolicyRoleExpr' referring to the database role with the given
name, with proper escaping as part of the generated SQL. E.G.

> "some_role"

@since 1.2.0.0
-}
namedPolicyRole :: String -> PolicyRoleExpr
namedPolicyRole =
  PolicyRoleExpr . RawSql.toRawSql . identifier

{- | The @PUBLIC@ role target, which makes the policy apply to all roles.

@since 1.2.0.0
-}
publicPolicyRole :: PolicyRoleExpr
publicPolicyRole =
  PolicyRoleExpr (RawSql.fromString "PUBLIC")

{- | The @CURRENT_ROLE@ role target. PostgreSQL resolves this to the concrete
current role at the time the statement is executed. Requires PostgreSQL 14 or
later.

@since 1.2.0.0
-}
currentRolePolicyRole :: PolicyRoleExpr
currentRolePolicyRole =
  PolicyRoleExpr (RawSql.fromString "CURRENT_ROLE")

{- | The @CURRENT_USER@ role target. PostgreSQL resolves this to the concrete
current role at the time the statement is executed.

@since 1.2.0.0
-}
currentUserPolicyRole :: PolicyRoleExpr
currentUserPolicyRole =
  PolicyRoleExpr (RawSql.fromString "CURRENT_USER")

{- | The @SESSION_USER@ role target. PostgreSQL resolves this to the concrete
session role at the time the statement is executed.

@since 1.2.0.0
-}
sessionUserPolicyRole :: PolicyRoleExpr
sessionUserPolicyRole =
  PolicyRoleExpr (RawSql.fromString "SESSION_USER")
