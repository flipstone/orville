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
  , PolicyPermissionExpr
  , PolicyCommandExpr
  , PolicyUsingExpr
  , PolicyCheckExpr
  , policyPermissive
  , policyRestrictive
  , policyCommandAll
  , policyCommandSelect
  , policyCommandInsert
  , policyCommandUpdate
  , policyCommandDelete
  , policyUsingExpr
  , policyCheckExpr
  , createPolicyExpr
  , alterPolicyExpr
  , comparePolicyUsingExpr
  , comparePolicyCheckExpr
  ) where

import qualified Data.ByteString as BS
import Data.Function (on)
import qualified Data.Word as Word

import Orville.PostgreSQL.Expr.Name (QualifiedOrUnqualified, TableName)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import Orville.PostgreSQL.Expr.Name (PolicyName)
import Orville.PostgreSQL.Expr.PolicyExpr.PolicyRole (PolicyRoleExpr)

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

{- | Type to represent the @PERMISSIVE@ or @RESTRICTIVE@ keyword used in the
@AS@ clause of a @CREATE POLICY@ statement. See 'policyPermissive' and
'policyRestrictive' for construction.

'PolicyPermissionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyPermissionExpr
  = PolicyPermissionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @PERMISSIVE@ keyword for the @AS@ clause of a @CREATE POLICY@
statement.

@since 1.2.0.0
-}
policyPermissive :: PolicyPermissionExpr
policyPermissive =
  PolicyPermissionExpr (RawSql.fromString "PERMISSIVE")

{- | The @RESTRICTIVE@ keyword for the @AS@ clause of a @CREATE POLICY@
statement.

@since 1.2.0.0
-}
policyRestrictive :: PolicyPermissionExpr
policyRestrictive =
  PolicyPermissionExpr (RawSql.fromString "RESTRICTIVE")

{- | Type to represent the command keyword used in the @FOR@ clause of a
@CREATE POLICY@ statement: @ALL@, @SELECT@, @INSERT@, @UPDATE@ or @DELETE@.
See 'policyCommandAll' and friends for construction.

'PolicyCommandExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyCommandExpr
  = PolicyCommandExpr RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @ALL@ command for the @FOR@ clause of a @CREATE POLICY@ statement.

@since 1.2.0.0
-}
policyCommandAll :: PolicyCommandExpr
policyCommandAll =
  PolicyCommandExpr (RawSql.fromString "ALL")

{- | The @SELECT@ command for the @FOR@ clause of a @CREATE POLICY@ statement.

@since 1.2.0.0
-}
policyCommandSelect :: PolicyCommandExpr
policyCommandSelect =
  PolicyCommandExpr (RawSql.fromString "SELECT")

{- | The @INSERT@ command for the @FOR@ clause of a @CREATE POLICY@ statement.

@since 1.2.0.0
-}
policyCommandInsert :: PolicyCommandExpr
policyCommandInsert =
  PolicyCommandExpr (RawSql.fromString "INSERT")

{- | The @UPDATE@ command for the @FOR@ clause of a @CREATE POLICY@ statement.

@since 1.2.0.0
-}
policyCommandUpdate :: PolicyCommandExpr
policyCommandUpdate =
  PolicyCommandExpr (RawSql.fromString "UPDATE")

{- | The @DELETE@ command for the @FOR@ clause of a @CREATE POLICY@ statement.

@since 1.2.0.0
-}
policyCommandDelete :: PolicyCommandExpr
policyCommandDelete =
  PolicyCommandExpr (RawSql.fromString "DELETE")

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

{- | Compares two 'PolicyUsingExpr' values by their example bytes, folding
ASCII case outside quoted regions so that expressions compare consistently
against the deparsed forms PostgreSQL returns in @pg_policies@. See
'caseFoldOutsideQuotes' for details.

This function is internal to Orville and is not exported from the public
modules.
-}
comparePolicyUsingExpr :: PolicyUsingExpr -> PolicyUsingExpr -> Ordering
comparePolicyUsingExpr =
  on compare (caseFoldOutsideQuotes . RawSql.toExampleBytes)

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

{- | Compares two 'PolicyCheckExpr' values by their example bytes, folding
ASCII case outside quoted regions so that expressions compare consistently
against the deparsed forms PostgreSQL returns in @pg_policies@. See
'caseFoldOutsideQuotes' for details.

This function is internal to Orville and is not exported from the public
modules.
-}
comparePolicyCheckExpr :: PolicyCheckExpr -> PolicyCheckExpr -> Ordering
comparePolicyCheckExpr =
  on compare (caseFoldOutsideQuotes . RawSql.toExampleBytes)

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
  optionally marking it as @PERMISSIVE@ or @RESTRICTIVE@, restricting it to a
  command and a set of roles and applying @USING@ and @WITH CHECK@
  expressions.

  If no 'PolicyPermissionExpr' is given, no @AS@ clause is included and
  PostgreSQL will use its default, which is @PERMISSIVE@ for current
  PostgreSQL versions. Likewise, if no 'PolicyCommandExpr' is given, no @FOR@
  clause is included and PostgreSQL will use its default, which is @ALL@.

@since 1.2.0.0
-}
createPolicyExpr ::
  PolicyName ->
  QualifiedOrUnqualified TableName ->
  Maybe PolicyPermissionExpr ->
  Maybe PolicyCommandExpr ->
  Maybe [PolicyRoleExpr] ->
  Maybe PolicyUsingExpr ->
  Maybe PolicyCheckExpr ->
  CreatePolicyExpr
createPolicyExpr name tableName mbPermission mbCommand mbRoles mbUsing mbCheck =
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
        ( \permission ->
            [ RawSql.fromString "AS"
            , RawSql.toRawSql permission
            ]
        )
        mbPermission
      <> maybe
        []
        ( \command ->
            [ RawSql.fromString "FOR"
            , RawSql.toRawSql command
            ]
        )
        mbCommand
      <> policyClauses mbRoles mbUsing mbCheck

{- | Constructs an 'AlterPolicyExpr' for the named policy on the given table,
  optionally restricting it to a set of roles and applying @USING@ and
  @WITH CHECK@ expressions.

  Note that @ALTER POLICY@ cannot change whether a policy is @PERMISSIVE@ or
  @RESTRICTIVE@, nor the command its @FOR@ clause names. Changing either
  requires dropping and recreating the policy.

@since 1.2.0.0
-}
alterPolicyExpr ::
  PolicyName ->
  QualifiedOrUnqualified TableName ->
  Maybe [PolicyRoleExpr] ->
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
      <> policyClauses mbRoles mbUsing mbCheck

{- | Renders the @TO@, @USING@ and @WITH CHECK@ clauses shared by
@CREATE POLICY@ and @ALTER POLICY@ statements.
-}
policyClauses ::
  Maybe [PolicyRoleExpr] ->
  Maybe PolicyUsingExpr ->
  Maybe PolicyCheckExpr ->
  [RawSql.RawSql]
policyClauses mbRoles mbUsing mbCheck =
  maybe
    []
    ( \roles ->
        [ RawSql.fromString "TO"
        , RawSql.intercalate RawSql.comma roles
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

{- | Folds ASCII uppercase bytes to lowercase everywhere except inside
single-quoted string literals and double-quoted identifiers. This mirrors
PostgreSQL's own lexical rules: keywords and unquoted identifiers are
case-insensitive (PostgreSQL renders them in its preferred case when
deparsing expressions for @pg_policies@), while case inside string literals
and quoted identifiers is significant and must be preserved so that policies
differing only there still compare as different.

A doubled quote inside a quoted region (@''@ or @\"\"@) is handled correctly
without lookahead: the first quote is treated as leaving the region and the
second as immediately re-entering it. Escape string syntax (@E'...'@) and
dollar quoting are not recognized; PostgreSQL never produces either when
deparsing policy expressions, so this only matters for hand-written SQL
using those forms.
-}
caseFoldOutsideQuotes :: BS.ByteString -> BS.ByteString
caseFoldOutsideQuotes =
  let
    step quoteState word =
      case quoteState of
        NotInQuote
          | word == singleQuote -> (InSingleQuote, word)
          | word == doubleQuote -> (InDoubleQuote, word)
          | otherwise -> (NotInQuote, toLowerWord8 word)
        InSingleQuote
          | word == singleQuote -> (NotInQuote, word)
          | otherwise -> (InSingleQuote, word)
        InDoubleQuote
          | word == doubleQuote -> (NotInQuote, word)
          | otherwise -> (InDoubleQuote, word)
  in
    snd . BS.mapAccumL step NotInQuote

data QuoteState
  = NotInQuote
  | InSingleQuote
  | InDoubleQuote

singleQuote :: Word.Word8
singleQuote = 39 -- '

doubleQuote :: Word.Word8
doubleQuote = 34 -- "

{- | Convert an ASCII uppercase byte to lowercase, leaving other bytes
(including UTF-8 continuation bytes) untouched.
-}
toLowerWord8 :: Word.Word8 -> Word.Word8
toLowerWord8 w
  | w >= 65 && w <= 90 = w + 32
  | otherwise = w
