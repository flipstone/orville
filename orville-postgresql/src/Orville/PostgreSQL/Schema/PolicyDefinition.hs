{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.PolicyDefinition
  ( PolicyDefinition
  , policyDefinitionPolicyName
  , policyDefinitionPolicyRoles
  , policyDefinitionUsingExpr
  , policyDefinitionCheckExpr
  , mkPolicyDefinition
  , mkCreatePolicyExpr
  , mkAlterPolicyExpr
  , mkDropPolicyExpr
  ) where

import qualified Data.List as List
import qualified Data.Set as Set

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier, tableIdQualifiedName)

{- | Defines a PostgreSQL row-level security policy that can be added to a table.

The Using and Check Exprs are rendered for comparison against PostgreSQL's @pg_policies@ view,
and must match the @qual@ and @with_check@ columns exactly, respectively.

@since 1.2.0.0
-}
data PolicyDefinition = PolicyDefinition
  { i_policyName :: String
  , i_policyRoles :: Maybe (Set.Set String)
  , i_usingExpr :: Maybe Expr.PolicyUsingExpr
  , i_checkExpr :: Maybe Expr.PolicyCheckExpr
  }
  deriving
    ( -- | @since 1.2.0.0
      Eq
    , -- | @since 1.2.0.0
      Ord
    )

{- | Constructs a 'PolicyDefinition' from a policy name and, optionally, the set
  of roles it applies to, a @USING@ expression and a @WITH CHECK@ expression.

@since 1.2.0.0
-}
mkPolicyDefinition ::
  String ->
  Maybe (Set.Set String) ->
  Maybe Expr.PolicyUsingExpr ->
  Maybe Expr.PolicyCheckExpr ->
  PolicyDefinition
mkPolicyDefinition name mbRoles mbUsing mbCheck =
  PolicyDefinition
    { i_policyName = name
    , i_policyRoles = mbRoles
    , i_usingExpr = mbUsing
    , i_checkExpr = mbCheck
    }

{- | Retrieves the name of the policy from a 'PolicyDefinition'.

@since 1.2.0.0
-}
policyDefinitionPolicyName :: PolicyDefinition -> String
policyDefinitionPolicyName =
  i_policyName

{- | Retrieves the set of roles the policy applies to, if any, from a
  'PolicyDefinition'. 'Nothing' indicates the policy applies to all roles.

@since 1.2.0.0
-}
policyDefinitionPolicyRoles :: PolicyDefinition -> Maybe (Set.Set String)
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

@since 1.2.0.0
-}
mkCreatePolicyExpr :: TableIdentifier -> PolicyDefinition -> Expr.CreatePolicyExpr
mkCreatePolicyExpr tableId policyDefinition =
  Expr.createPolicyExpr
    (Expr.policyName $ policyDefinitionPolicyName policyDefinition)
    (tableIdQualifiedName tableId)
    (List.map Expr.policyRole . Set.toList <$> i_policyRoles policyDefinition)
    (i_usingExpr policyDefinition)
    (i_checkExpr policyDefinition)

{- | Builds the 'Expr.AlterPolicyExpr' that will update the given policy on the
  table identified by the 'TableIdentifier' to match the 'PolicyDefinition'.

@since 1.2.0.0
-}
mkAlterPolicyExpr :: TableIdentifier -> PolicyDefinition -> Expr.AlterPolicyExpr
mkAlterPolicyExpr tableId policyDefinition =
  Expr.alterPolicyExpr
    (Expr.policyName $ policyDefinitionPolicyName policyDefinition)
    (tableIdQualifiedName tableId)
    (List.map Expr.policyRole . Set.toList <$> i_policyRoles policyDefinition)
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
