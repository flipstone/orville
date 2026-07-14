{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.PolicyDefinition
  ( PolicyDefinition
  , PolicyPermission (PolicyPermissive, PolicyRestrictive)
  , PolicyCommand (PolicyCommandAll, PolicyCommandSelect, PolicyCommandInsert, PolicyCommandUpdate, PolicyCommandDelete)
  , PolicyRole (PolicyRolePublic, PolicyRoleCurrentRole, PolicyRoleCurrentUser, PolicyRoleSessionUser, PolicyRoleNamed)
  , policyDefinitionPolicyName
  , policyDefinitionPermission
  , policyDefinitionCommand
  , policyDefinitionPolicyRoles
  , policyDefinitionUsingExpr
  , policyDefinitionCheckExpr
  , mkPolicyDefinition
  , mkCreatePolicyExpr
  , mkAlterPolicyExpr
  , mkDropPolicyExpr
  ) where

import qualified Data.Functor.Classes as Classes
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Expr.PolicyExpr.CreatePolicyExpr as CreatePolicyExpr
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier, tableIdQualifiedName)

{- | Defines a PostgreSQL row-level security policy that can be added to a table.

  Auto-migration compares the @USING@ and @WITH CHECK@ expressions against the
  @qual@ and @with_check@ columns of PostgreSQL's @pg_policies@ view,
  case-insensitively outside of string literals and quoted identifiers. Those
  columns hold the expressions as PostgreSQL deparses them — with the
  parentheses it adds around operator expressions, explicit casts such as
  @::text@, and identifiers quoted only where necessary — so expressions
  should be written in that form. An expression that differs from the
  deparsed form only cosmetically will cause auto-migration to drop and
  re-create the policy on every run.

@since 1.2.0.0
-}
data PolicyDefinition = PolicyDefinition
  { i_policyName :: String
  , i_policyPermission :: PolicyPermission
  , i_policyCommand :: PolicyCommand
  , i_policyRoles :: Set.Set PolicyRole
  , i_usingExpr :: Maybe Expr.PolicyUsingExpr
  , i_checkExpr :: Maybe Expr.PolicyCheckExpr
  }

-- | @since 1.2.0.0
instance Eq PolicyDefinition where
  left == right = comparePolicyDefinition left right == EQ

-- | @since 1.2.0.0
instance Ord PolicyDefinition where
  compare = comparePolicyDefinition

{- | Compares 'PolicyDefinition's field by field, using
  'CreatePolicyExpr.comparePolicyUsingExpr' and
  'CreatePolicyExpr.comparePolicyCheckExpr' for the @USING@ and @WITH CHECK@
  expressions so that definitions compare consistently against policies
  loaded from PostgreSQL's @pg_policies@ view.
-}
comparePolicyDefinition :: PolicyDefinition -> PolicyDefinition -> Ordering
comparePolicyDefinition left right =
  compare (i_policyName left) (i_policyName right)
    <> compare (i_policyPermission left) (i_policyPermission right)
    <> compare (i_policyCommand left) (i_policyCommand right)
    <> compare (i_policyRoles left) (i_policyRoles right)
    <> Classes.liftCompare CreatePolicyExpr.comparePolicyUsingExpr (i_usingExpr left) (i_usingExpr right)
    <> Classes.liftCompare CreatePolicyExpr.comparePolicyCheckExpr (i_checkExpr left) (i_checkExpr right)

{- | Indicates whether a policy is permissive or restrictive. Rows must pass at
  least one permissive policy and every restrictive policy that applies to a
  query.

@since 1.2.0.0
-}
data PolicyPermission
  = PolicyPermissive
  | PolicyRestrictive
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    , -- | @since 1.2.0.0
      Show
    )

{- | The command that a policy applies to, as named in the @FOR@ clause of
  @CREATE POLICY@. 'PolicyCommandAll' applies the policy to all commands.

  Note that PostgreSQL does not allow @WITH CHECK@ expressions on @SELECT@ or
  @DELETE@ policies, nor @USING@ expressions on @INSERT@ policies.

@since 1.2.0.0
-}
data PolicyCommand
  = PolicyCommandAll
  | PolicyCommandSelect
  | PolicyCommandInsert
  | PolicyCommandUpdate
  | PolicyCommandDelete
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    , -- | @since 1.2.0.0
      Show
    )

{- | A role target that a policy applies to: a named database role or one of
  the special targets PostgreSQL accepts in the @TO@ clause of
  @CREATE POLICY@.

  Note that PostgreSQL resolves 'PolicyRoleCurrentRole', 'PolicyRoleCurrentUser'
  and 'PolicyRoleSessionUser' to the concrete role at the time the policy is
  created or altered, and reports the resolved role name in @pg_policies@.
  Auto-migration compares policy definitions against @pg_policies@, so a
  definition using one of these special targets will never match the existing
  policy and will drop and re-create the policy on every migration run. Use
  'PolicyRoleNamed' for policies managed by auto-migration.

  'PolicyRoleCurrentRole' requires PostgreSQL 14 or later.

@since 1.2.0.0
-}
data PolicyRole
  = PolicyRolePublic
  | PolicyRoleCurrentRole
  | PolicyRoleCurrentUser
  | PolicyRoleSessionUser
  | PolicyRoleNamed String
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    , -- | @since 1.2.0.0
      Show
    )

{- | Constructs a 'PolicyDefinition' from a policy name and, optionally, a
  'PolicyPermission', a 'PolicyCommand', the set of roles it applies to, a
  @USING@ expression and a @WITH CHECK@ expression.

  If no 'PolicyPermission' is given, the policy is treated as
  'PolicyPermissive', which is the default behavior for current PostgreSQL
  versions when creating a policy. Orville includes the resulting
  @AS PERMISSIVE@ or @AS RESTRICTIVE@ clause explicitly in the generated
  @CREATE POLICY@ statement rather than relying on the PostgreSQL default.

  Similarly, if no 'PolicyCommand' is given, the policy is treated as
  'PolicyCommandAll', and if no roles (or an empty set) are given, the policy
  is treated as applying to 'PolicyRolePublic'. Both match the default
  behavior for current PostgreSQL versions when the @FOR@ or @TO@ clause is
  omitted, and Orville includes the resulting clauses explicitly in the
  generated SQL.

  The role set is also normalized to mirror how PostgreSQL treats role
  targets, so that definitions compare consistently against @pg_policies@:
  a 'PolicyRoleNamed' @"public"@ is the PUBLIC pseudo-role and becomes
  'PolicyRolePublic', and a set containing 'PolicyRolePublic' collapses to
  just 'PolicyRolePublic' (PostgreSQL ignores any other role targets given
  alongside @PUBLIC@, since all roles are members of it).

@since 1.2.0.0
-}
mkPolicyDefinition ::
  String ->
  Maybe PolicyPermission ->
  Maybe PolicyCommand ->
  Maybe (Set.Set PolicyRole) ->
  Maybe Expr.PolicyUsingExpr ->
  Maybe Expr.PolicyCheckExpr ->
  PolicyDefinition
mkPolicyDefinition name mbPermission mbCommand mbRoles mbUsing mbCheck =
  let
    normalizeRole role =
      case role of
        PolicyRoleNamed "public" -> PolicyRolePublic
        _ -> role

    roles = Set.map normalizeRole (Maybe.fromMaybe Set.empty mbRoles)
  in
    PolicyDefinition
      { i_policyName = name
      , i_policyPermission = Maybe.fromMaybe PolicyPermissive mbPermission
      , i_policyCommand = Maybe.fromMaybe PolicyCommandAll mbCommand
      , i_policyRoles =
          if Set.null roles || Set.member PolicyRolePublic roles
            then Set.singleton PolicyRolePublic
            else roles
      , i_usingExpr = mbUsing
      , i_checkExpr = mbCheck
      }

{- | Retrieves the name of the policy from a 'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionPolicyName :: PolicyDefinition -> String
policyDefinitionPolicyName =
  i_policyName

{- | Retrieves whether the policy is permissive or restrictive from a
  'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionPermission :: PolicyDefinition -> PolicyPermission
policyDefinitionPermission =
  i_policyPermission

{- | Retrieves the command the policy applies to from a 'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionCommand :: PolicyDefinition -> PolicyCommand
policyDefinitionCommand =
  i_policyCommand

{- | Retrieves the set of roles the policy applies to from a
  'PolicyDefinition'. A policy constructed without any roles applies to
  'PolicyRolePublic'.

@since 1.2.0.0
-}
policyDefinitionPolicyRoles :: PolicyDefinition -> Set.Set PolicyRole
policyDefinitionPolicyRoles =
  i_policyRoles

{- | Retrieves the @USING@ expression of the policy, if any, from a
  'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionUsingExpr :: PolicyDefinition -> Maybe Expr.PolicyUsingExpr
policyDefinitionUsingExpr =
  i_usingExpr

{- | Retrieves the @WITH CHECK@ expression of the policy, if any, from a
  'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionCheckExpr :: PolicyDefinition -> Maybe Expr.PolicyCheckExpr
policyDefinitionCheckExpr =
  i_checkExpr

{- | Builds the 'Expr.CreatePolicyExpr' that will create the given policy on the
  table identified by the 'TableIdentifier'.

  The policy's 'PolicyPermission', 'PolicyCommand' and 'PolicyRole's are
  always included explicitly as @AS@, @FOR@ and @TO@ clauses.

@since 1.2.0.0
-}
mkCreatePolicyExpr :: TableIdentifier -> PolicyDefinition -> Expr.CreatePolicyExpr
mkCreatePolicyExpr tableId policyDefinition =
  Expr.createPolicyExpr
    (Expr.policyName $ policyDefinitionPolicyName policyDefinition)
    (tableIdQualifiedName tableId)
    (Just . policyPermissionExpr . policyDefinitionPermission $ policyDefinition)
    (Just . policyCommandExpr . policyDefinitionCommand $ policyDefinition)
    (NEL.nonEmpty . List.map policyRoleExpr . Set.toList $ i_policyRoles policyDefinition)
    (i_usingExpr policyDefinition)
    (i_checkExpr policyDefinition)

policyPermissionExpr :: PolicyPermission -> Expr.PolicyPermissionExpr
policyPermissionExpr permission =
  case permission of
    PolicyPermissive -> Expr.policyPermissive
    PolicyRestrictive -> Expr.policyRestrictive

policyCommandExpr :: PolicyCommand -> Expr.PolicyCommandExpr
policyCommandExpr command =
  case command of
    PolicyCommandAll -> Expr.policyCommandAll
    PolicyCommandSelect -> Expr.policyCommandSelect
    PolicyCommandInsert -> Expr.policyCommandInsert
    PolicyCommandUpdate -> Expr.policyCommandUpdate
    PolicyCommandDelete -> Expr.policyCommandDelete

policyRoleExpr :: PolicyRole -> Expr.PolicyRoleExpr
policyRoleExpr role =
  case role of
    PolicyRolePublic -> Expr.publicPolicyRole
    PolicyRoleCurrentRole -> Expr.currentRolePolicyRole
    PolicyRoleCurrentUser -> Expr.currentUserPolicyRole
    PolicyRoleSessionUser -> Expr.sessionUserPolicyRole
    PolicyRoleNamed name -> Expr.namedPolicyRole name

{- | Builds the 'Expr.AlterPolicyExpr' that will update the given policy on the
  table identified by the 'TableIdentifier' to match the 'PolicyDefinition'.

  Note that @ALTER POLICY@ cannot change whether a policy is permissive or
  restrictive, nor the command it applies to, so the 'PolicyPermission' and
  'PolicyCommand' of the definition are ignored here. Changing either
  requires dropping and recreating the policy.

@since 1.2.0.0
-}
mkAlterPolicyExpr :: TableIdentifier -> PolicyDefinition -> Expr.AlterPolicyExpr
mkAlterPolicyExpr tableId policyDefinition =
  Expr.alterPolicyExpr
    (Expr.policyName $ policyDefinitionPolicyName policyDefinition)
    (tableIdQualifiedName tableId)
    (NEL.nonEmpty . List.map policyRoleExpr . Set.toList $ i_policyRoles policyDefinition)
    (i_usingExpr policyDefinition)
    (i_checkExpr policyDefinition)

{- | Builds the 'Expr.DropPolicyExpr' that will drop the given policy from the
  table identified by the 'TableIdentifier' if it exists.

@since 1.2.0.0
-}
mkDropPolicyExpr :: TableIdentifier -> PolicyDefinition -> Expr.DropPolicyExpr
mkDropPolicyExpr tableId policyDefinition =
  Expr.dropPolicy
    (Just Expr.ifExists)
    (Expr.policyName $ policyDefinitionPolicyName policyDefinition)
    (tableIdQualifiedName tableId)
