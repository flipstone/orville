{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.PolicyExpr.DropPolicyExpr
  ( DropPolicyExpr
  , dropPolicy
  ) where

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (PolicyName, QualifiedOrUnqualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL @DROP POLICY@ statement. E.G.

> DROP POLICY IF EXISTS "some_policy" ON "some_table"

'DropPolicyExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype DropPolicyExpr
  = DropPolicyExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'DropPolicyExpr' that will drop the named policy from the
  given table, optionally only if it exists.

@since 1.2.0.0
-}
dropPolicy ::
  Maybe IfExists ->
  PolicyName ->
  QualifiedOrUnqualified TableName ->
  DropPolicyExpr
dropPolicy maybeIfExists policyName tableName =
  DropPolicyExpr
    $ RawSql.intercalate
      RawSql.space
    $ [RawSql.fromString "DROP POLICY"]
      <> maybe [] (pure . RawSql.toRawSql) maybeIfExists
      <> [ RawSql.toRawSql policyName
         , RawSql.fromString "ON"
         , RawSql.toRawSql tableName
         ]
