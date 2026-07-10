module Test.Expr.Policy
  ( policyTests
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

policyTests :: Orville.ConnectionPool -> Property.Group
policyTests pool =
  Property.group
    "Expr - Policy"
    [ prop_createPolicyWithAllClauses pool
    , prop_specialRoleTargets pool
    , prop_alterPolicy pool
    , prop_dropPolicyIfExists pool
    ]

prop_createPolicyWithAllClauses :: Property.NamedDBProperty
prop_createPolicyWithAllClauses =
  Property.singletonNamedDBProperty "creates a policy with AS, TO, USING and WITH CHECK clauses" $ \pool -> do
    let
      createPolicy =
        Expr.createPolicyExpr
          (Expr.policyName "expr_test_policy")
          testTableName
          (Just Expr.policyRestrictive)
          (Just Expr.policyCommandUpdate)
          (Just [Expr.namedPolicyRole "orville_test"])
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True)
          (Just . Expr.policyCheckExpr $ Expr.literalBooleanExpr False)

    policies <-
      HH.evalIO $
        Orville.runOrville pool $ do
          recreateTestTable
          Orville.executeVoid Orville.DDLQuery createPolicy
          findTestPolicies

    fmap policyRowPolicyName policies === [T.pack "expr_test_policy"]
    fmap policyRowPermissive policies === [T.pack "RESTRICTIVE"]
    fmap policyRowCmd policies === [T.pack "UPDATE"]
    fmap policyRowRoles policies === [T.pack "{orville_test}"]

prop_specialRoleTargets :: Property.NamedDBProperty
prop_specialRoleTargets =
  Property.singletonNamedDBProperty "creates policies with the special role targets" $ \pool -> do
    let
      mkPolicy name role =
        Expr.createPolicyExpr
          (Expr.policyName name)
          testTableName
          Nothing
          Nothing
          (Just [role])
          Nothing
          Nothing

    policies <-
      HH.evalIO $
        Orville.runOrville pool $ do
          recreateTestTable
          Orville.executeVoid Orville.DDLQuery $ mkPolicy "policy_a_current_role" Expr.currentRolePolicyRole
          Orville.executeVoid Orville.DDLQuery $ mkPolicy "policy_b_current_user" Expr.currentUserPolicyRole
          Orville.executeVoid Orville.DDLQuery $ mkPolicy "policy_c_session_user" Expr.sessionUserPolicyRole
          Orville.executeVoid Orville.DDLQuery $ mkPolicy "policy_d_public" Expr.publicPolicyRole
          findTestPolicies

    -- PostgreSQL resolves CURRENT_ROLE, CURRENT_USER and SESSION_USER to the
    -- concrete role at the time the policy is created. The test suite
    -- connects as "orville_test".
    fmap policyRowRoles (List.sortOn policyRowPolicyName policies)
      === fmap
        T.pack
        [ "{orville_test}"
        , "{orville_test}"
        , "{orville_test}"
        , "{public}"
        ]

prop_alterPolicy :: Property.NamedDBProperty
prop_alterPolicy =
  Property.singletonNamedDBProperty "alters the roles and expressions of a policy" $ \pool -> do
    let
      createPolicy =
        Expr.createPolicyExpr
          (Expr.policyName "expr_test_policy")
          testTableName
          Nothing
          Nothing
          (Just [Expr.namedPolicyRole "orville_test"])
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True)
          Nothing

      alterPolicy =
        Expr.alterPolicyExpr
          (Expr.policyName "expr_test_policy")
          testTableName
          (Just [Expr.publicPolicyRole])
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr False)
          (Just . Expr.policyCheckExpr $ Expr.literalBooleanExpr True)

    policies <-
      HH.evalIO $
        Orville.runOrville pool $ do
          recreateTestTable
          Orville.executeVoid Orville.DDLQuery createPolicy
          Orville.executeVoid Orville.DDLQuery alterPolicy
          findTestPolicies

    fmap policyRowRoles policies === [T.pack "{public}"]

prop_dropPolicyIfExists :: Property.NamedDBProperty
prop_dropPolicyIfExists =
  Property.singletonNamedDBProperty "drops policies, tolerating missing ones with IF EXISTS" $ \pool -> do
    let
      createPolicy =
        Expr.createPolicyExpr
          (Expr.policyName "expr_test_policy")
          testTableName
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing

    policies <-
      HH.evalIO $
        Orville.runOrville pool $ do
          recreateTestTable
          Orville.executeVoid Orville.DDLQuery $
            Expr.dropPolicy (Just Expr.ifExists) (Expr.policyName "nonexistent_policy") testTableName
          Orville.executeVoid Orville.DDLQuery createPolicy
          Orville.executeVoid Orville.DDLQuery $
            Expr.dropPolicy Nothing (Expr.policyName "expr_test_policy") testTableName
          findTestPolicies

    fmap policyRowPolicyName policies === []

testTableName :: Expr.QualifiedOrUnqualified Expr.TableName
testTableName =
  Expr.unqualified (Expr.tableName "expr_policy_test")

recreateTestTable :: Orville.Orville ()
recreateTestTable = do
  Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP TABLE IF EXISTS expr_policy_test"
  Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE TABLE expr_policy_test (name TEXT)"

findTestPolicies :: Orville.Orville [PolicyRow]
findTestPolicies =
  Orville.findEntitiesBy pgPoliciesTable . Orville.where_ $
    Orville.fieldEquals (Orville.unboundedTextField "tablename") (T.pack "expr_policy_test")

data PolicyRow = PolicyRow
  { policyRowPolicyName :: T.Text
  , policyRowPermissive :: T.Text
  , policyRowCmd :: T.Text
  , policyRowRoles :: T.Text
  }

pgPoliciesTable :: Orville.TableDefinition Orville.NoKey PolicyRow PolicyRow
pgPoliciesTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey "pg_policies" policyRowMarshaller

policyRowMarshaller :: Orville.SqlMarshaller w PolicyRow
policyRowMarshaller =
  PolicyRow
    <$> Orville.marshallReadOnlyField (Orville.unboundedTextField "policyname")
    <*> Orville.marshallReadOnlyField (Orville.unboundedTextField "permissive")
    <*> Orville.marshallReadOnlyField (Orville.unboundedTextField "cmd")
    <*> Orville.marshallReadOnlyField (Orville.unboundedTextField "roles")
