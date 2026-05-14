{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.PolicyExpr.CreatePolicyExpr
  ( CreatePolicyExpr
  , AlterPolicyExpr
  , PolicyUsingExpr
  , PolicyCheckExpr
  , policyUsingExpr
  , policyCheckExpr
  , createPolicyExpr
  , alterPolicyExpr
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.Word as Word

import Orville.PostgreSQL.Expr.Name (QualifiedOrUnqualified, TableName)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import Orville.PostgreSQL.Expr.Name (PolicyName)
import Orville.PostgreSQL.Expr.PolicyExpr.PolicyRole (PolicyRole)

{- | Type to represent a SQL @CREATE POLICY@ statement. E.G.

> CREATE POLICY "some_policy" ON "some_table" TO "some_role" USING (...) WITH CHECK (...)

'CreatePolicyExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype CreatePolicyExpr
  = CreatePolicyExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Type to represent a SQL @ALTER POLICY@ statement. E.G.

> ALTER POLICY "some_policy" ON "some_table" TO "some_role" USING (...) WITH CHECK (...)

'AlterPolicyExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AlterPolicyExpr
  = AlterPolicyExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Type to represent the expression used in the @USING@ clause of a policy,
which restricts the rows visible to existing-row commands. See
'policyUsingExpr' for construction.

'PolicyUsingExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyUsingExpr
  = PolicyUsingExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

-- | @since 1.2.0.0
instance Eq PolicyUsingExpr where
  (==) = on (==) (BS.map toLowerWord8 . RawSql.toExampleBytes)

-- | @since 1.2.0.0
instance Ord PolicyUsingExpr where
  compare = on compare (BS.map toLowerWord8 . RawSql.toExampleBytes)

{- | Type to represent the expression used in the @WITH CHECK@ clause of a
policy, which restricts the rows that new-row commands may produce. See
'policyCheckExpr' for construction.

'PolicyCheckExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyCheckExpr
  = PolicyCheckExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

-- | @since 1.2.0.0
instance Eq PolicyCheckExpr where
  (==) = on (==) (BS.map toLowerWord8 . RawSql.toExampleBytes)

-- | @since 1.2.0.0
instance Ord PolicyCheckExpr where
  compare = on compare (BS.map toLowerWord8 . RawSql.toExampleBytes)

{- | Constructs a 'PolicyUsingExpr' from a 'BooleanExpr'.

Note that this parenthesizes the 'BooleanExpr' in order to match the @qual@
column in @pg_policies@.

@since 1.2.0.0
-}
policyUsingExpr :: BooleanExpr -> PolicyUsingExpr
policyUsingExpr = PolicyUsingExpr . RawSql.parenthesized

{- | Constructs a 'PolicyCheckExpr' from a 'BooleanExpr'.

Note that this parenthesizes the 'BooleanExpr' in order to match the
@with_check@ column in @pg_policies@.

@since 1.2.0.0
-}
policyCheckExpr :: BooleanExpr -> PolicyCheckExpr
policyCheckExpr = PolicyCheckExpr . RawSql.parenthesized

{- | Constructs a 'CreatePolicyExpr' for the named policy on the given table,
  optionally restricting it to a set of roles and applying @USING@ and
  @WITH CHECK@ expressions.

@since 1.2.0.0
-}
createPolicyExpr ::
  PolicyName ->
  QualifiedOrUnqualified TableName ->
  Maybe [PolicyRole] ->
  Maybe PolicyUsingExpr ->
  Maybe PolicyCheckExpr ->
  CreatePolicyExpr
createPolicyExpr name tableName mbRoles mbUsing mbCheck =
  CreatePolicyExpr
    $ RawSql.intercalate
      RawSql.space
    $ [ RawSql.fromString "CREATE POLICY"
      , RawSql.toRawSql name
      , RawSql.fromString "ON"
      , RawSql.toRawSql tableName
      ]
      <> maybe
        []
        ( \roles ->
            [ RawSql.fromString "TO"
            , RawSql.toRawSql . RawSql.intercalate RawSql.comma $ roles
            ]
        )
        mbRoles
      <> maybe
        []
        ( \using ->
            [ RawSql.fromString "USING"
            , RawSql.toRawSql using
            ]
        )
        mbUsing
      <> maybe
        []
        ( \check ->
            [ RawSql.fromString "WITH CHECK"
            , RawSql.toRawSql check
            ]
        )
        mbCheck

{- | Constructs an 'AlterPolicyExpr' for the named policy on the given table,
  optionally restricting it to a set of roles and applying @USING@ and
  @WITH CHECK@ expressions.

@since 1.2.0.0
-}
alterPolicyExpr ::
  PolicyName ->
  QualifiedOrUnqualified TableName ->
  Maybe [PolicyRole] ->
  Maybe PolicyUsingExpr ->
  Maybe PolicyCheckExpr ->
  AlterPolicyExpr
alterPolicyExpr name tableName mbRoles mbUsing mbCheck =
  AlterPolicyExpr
    $ RawSql.intercalate
      RawSql.space
    $ [ RawSql.fromString "ALTER POLICY"
      , RawSql.toRawSql name
      , RawSql.fromString "ON"
      , RawSql.toRawSql tableName
      ]
      <> maybe
        []
        ( \roles ->
            [ RawSql.fromString "TO"
            , RawSql.toRawSql . RawSql.intercalate RawSql.comma $ roles
            ]
        )
        mbRoles
      <> maybe
        []
        ( \using ->
            [ RawSql.fromString "USING"
            , RawSql.toRawSql using
            ]
        )
        mbUsing
      <> maybe
        []
        ( \check ->
            [ RawSql.fromString "WITH CHECK"
            , RawSql.toRawSql check
            ]
        )
        mbCheck

{- | Convert ASCII uppercase bytes to lowercase for case-insensitive comparison.
PostgreSQL normalizes SQL keywords to lowercase in pg_policies, so we need
case-insensitive comparison to avoid spurious ALTER POLICY steps.
-}
toLowerWord8 :: Word.Word8 -> Word.Word8
toLowerWord8 w
  | w >= 65 && w <= 90 = w + 32
  | otherwise = w
