{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgPolicy
  ( PgPolicy (..)
  , pgPoliciesTable
  , pgPolicySchemaNameField
  , pgPolicyTableNameField
  , pgPolicyPolicyNameField
  , pgPolicyPermissiveField
  , pgPolicyCmdField
  , pgPolicyRolesField
  , pgPolicyQualField
  , pgPolicyWithCheckField
  ) where

import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Internal.PgArrayText as PgArrayText

{- | The Haskell representation of data read from the @pg_catalog.pg_policies@
  view. Rows in this view correspond to row-level security policies.

  This models the @pg_policies@ view rather than the underlying @pg_policy@
  catalog table because the view exposes the policy's @USING@ and
  @WITH CHECK@ expressions as deparsed SQL text (via @pg_get_expr@) and its
  roles as names rather than oids. Note that the view carries no oids, so
  rows are identified by schema, table and policy name.

@since 1.2.0.0
-}
data PgPolicy = PgPolicy
  { pgPolicySchemaName :: T.Text
  -- ^ The name of the schema containing the table the policy is on.
  , pgPolicyTableName :: T.Text
  -- ^ The name of the table the policy is on.
  , pgPolicyPolicyName :: T.Text
  -- ^ The name of the policy.
  , pgPolicyPermissive :: Orville.PolicyPermission
  -- ^ Whether the policy is permissive or restrictive.
  , pgPolicyCmd :: Orville.PolicyCommand
  -- ^ The command the policy applies to.
  , pgPolicyRoles :: [T.Text]
  {- ^ The names of the roles the policy applies to. Policies that apply to
  all roles are reported as applying to the single role @public@.
  -}
  , pgPolicyQual :: Maybe T.Text
  -- ^ The deparsed text of the policy's @USING@ expression, if any.
  , pgPolicyWithCheck :: Maybe T.Text
  -- ^ The deparsed text of the policy's @WITH CHECK@ expression, if any.
  }

{- | An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPoliciesTable :: Orville.TableDefinition Orville.NoKey PgPolicy PgPolicy
pgPoliciesTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey "pg_policies" pgPolicyMarshaller

pgPolicyMarshaller :: Orville.SqlMarshaller w PgPolicy
pgPolicyMarshaller =
  PgPolicy
    <$> Orville.marshallReadOnlyField pgPolicySchemaNameField
    <*> Orville.marshallReadOnlyField pgPolicyTableNameField
    <*> Orville.marshallReadOnlyField pgPolicyPolicyNameField
    <*> Orville.marshallReadOnlyField pgPolicyPermissiveField
    <*> Orville.marshallReadOnlyField pgPolicyCmdField
    <*> Orville.marshallReadOnlyField pgPolicyRolesField
    <*> Orville.marshallReadOnlyField pgPolicyQualField
    <*> Orville.marshallReadOnlyField pgPolicyWithCheckField

{- | The @schemaname@ column of the @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPolicySchemaNameField :: Orville.FieldDefinition Orville.NotNull T.Text
pgPolicySchemaNameField =
  Orville.unboundedTextField "schemaname"

{- | The @tablename@ column of the @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPolicyTableNameField :: Orville.FieldDefinition Orville.NotNull T.Text
pgPolicyTableNameField =
  Orville.unboundedTextField "tablename"

{- | The @policyname@ column of the @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPolicyPolicyNameField :: Orville.FieldDefinition Orville.NotNull T.Text
pgPolicyPolicyNameField =
  Orville.unboundedTextField "policyname"

{- | The @permissive@ column of the @pg_catalog.pg_policies@ view. Values
  other than @PERMISSIVE@ or @RESTRICTIVE@ fail to decode.

@since 1.2.0.0
-}
pgPolicyPermissiveField :: Orville.FieldDefinition Orville.NotNull Orville.PolicyPermission
pgPolicyPermissiveField =
  Orville.convertField
    (Orville.tryConvertSqlType policyPermissionToPgText pgTextToPolicyPermission)
    (Orville.unboundedTextField "permissive")

{- | Converts an 'Orville.PolicyPermission' to the textual representation used
  in the @permissive@ column of the @pg_catalog.pg_policies@ view.

  See also 'pgTextToPolicyPermission'.
-}
policyPermissionToPgText :: Orville.PolicyPermission -> T.Text
policyPermissionToPgText permission =
  T.pack $
    case permission of
      Orville.PolicyPermissive -> "PERMISSIVE"
      Orville.PolicyRestrictive -> "RESTRICTIVE"

{- | Attempts to parse a value from the @permissive@ column of the
  @pg_catalog.pg_policies@ view as an 'Orville.PolicyPermission'.

  See also 'policyPermissionToPgText'.
-}
pgTextToPolicyPermission :: T.Text -> Either String Orville.PolicyPermission
pgTextToPolicyPermission text =
  case T.unpack (T.toUpper text) of
    "PERMISSIVE" -> Right Orville.PolicyPermissive
    "RESTRICTIVE" -> Right Orville.PolicyRestrictive
    other -> Left ("Unrecognized PostgreSQL policy permissive value: " <> other)

{- | The @cmd@ column of the @pg_catalog.pg_policies@ view. Values other than
  @ALL@, @SELECT@, @INSERT@, @UPDATE@ or @DELETE@ fail to decode.

@since 1.2.0.0
-}
pgPolicyCmdField :: Orville.FieldDefinition Orville.NotNull Orville.PolicyCommand
pgPolicyCmdField =
  Orville.convertField
    (Orville.tryConvertSqlType policyCommandToPgText pgTextToPolicyCommand)
    (Orville.unboundedTextField "cmd")

{- | Converts an 'Orville.PolicyCommand' to the textual representation used in
  the @cmd@ column of the @pg_catalog.pg_policies@ view.

  See also 'pgTextToPolicyCommand'.
-}
policyCommandToPgText :: Orville.PolicyCommand -> T.Text
policyCommandToPgText command =
  T.pack $
    case command of
      Orville.PolicyCommandAll -> "ALL"
      Orville.PolicyCommandSelect -> "SELECT"
      Orville.PolicyCommandInsert -> "INSERT"
      Orville.PolicyCommandUpdate -> "UPDATE"
      Orville.PolicyCommandDelete -> "DELETE"

{- | Attempts to parse a value from the @cmd@ column of the
  @pg_catalog.pg_policies@ view as an 'Orville.PolicyCommand'.

  See also 'policyCommandToPgText'.
-}
pgTextToPolicyCommand :: T.Text -> Either String Orville.PolicyCommand
pgTextToPolicyCommand text =
  case T.unpack (T.toUpper text) of
    "ALL" -> Right Orville.PolicyCommandAll
    "SELECT" -> Right Orville.PolicyCommandSelect
    "INSERT" -> Right Orville.PolicyCommandInsert
    "UPDATE" -> Right Orville.PolicyCommandUpdate
    "DELETE" -> Right Orville.PolicyCommandDelete
    other -> Left ("Unrecognized PostgreSQL policy command value: " <> other)

{- | The @roles@ column of the @pg_catalog.pg_policies@ view, a @name[]@
  column marshalled via its text representation.

@since 1.2.0.0
-}
pgPolicyRolesField :: Orville.FieldDefinition Orville.NotNull [T.Text]
pgPolicyRolesField =
  Orville.convertField
    (Orville.tryConvertSqlType PgArrayText.textListToPgArrayText (PgArrayText.pgArrayTextToTextList "role name list"))
    (Orville.unboundedTextField "roles")

{- | The @qual@ column of the @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPolicyQualField :: Orville.FieldDefinition Orville.Nullable (Maybe T.Text)
pgPolicyQualField =
  Orville.nullableField $ Orville.unboundedTextField "qual"

{- | The @with_check@ column of the @pg_catalog.pg_policies@ view.

@since 1.2.0.0
-}
pgPolicyWithCheckField :: Orville.FieldDefinition Orville.Nullable (Maybe T.Text)
pgPolicyWithCheckField =
  Orville.nullableField $ Orville.unboundedTextField "with_check"
