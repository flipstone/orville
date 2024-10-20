module Test.PgCatalog
  ( pgCatalogTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

pgCatalogTests :: Orville.ConnectionPool -> Property.Group
pgCatalogTests pool =
  Property.group
    "PgCatalog"
    [ prop_queryPgClass pool
    , prop_queryPgAttribute pool
    , prop_queryPgAttributeDefault pool
    , prop_queryPgConstraint pool
    , prop_queryPgTrigger pool
    , prop_queryPgProc pool
    , prop_describeDatabaseRelations pool
    ]

prop_queryPgClass :: Property.NamedDBProperty
prop_queryPgClass =
  Property.singletonNamedDBProperty "Can query the pg_class table to find out about a table" $ \pool -> do
    result <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField pgClass)

    fmap PgCatalog.pgClassRelationName result === Just pgClass
    fmap PgCatalog.pgClassRelationKind result === Just PgCatalog.OrdinaryTable

prop_queryPgAttribute :: Property.NamedDBProperty
prop_queryPgAttribute =
  Property.singletonNamedDBProperty "Can query the pg_attribute table to find out about a column" $ \pool -> do
    maybePgClassRecord <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField pgClass)

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    maybePgAttr <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgAttributeTable
        ( Orville.where_ $
            Orville.andExpr
              (Orville.fieldEquals PgCatalog.attributeRelationOidField pgClassOid)
              (Orville.fieldEquals PgCatalog.attributeNameField relname)
        )

    fmap PgCatalog.pgAttributeName maybePgAttr === Just relname
    fmap PgCatalog.pgAttributeLength maybePgAttr === Just 64

prop_queryPgAttributeDefault :: Property.NamedDBProperty
prop_queryPgAttributeDefault =
  Property.singletonNamedDBProperty "Can query the pg_attrdef table to find out about a default value" $ \pool -> do
    let
      fieldDefWithDefault =
        Orville.setDefaultValue (Orville.integerDefault 0) (Orville.integerField "foo")

      tableDef =
        Orville.mkTableDefinitionWithoutKey
          "test_pg_attrdef_query"
          (Orville.marshallField id fieldDefWithDefault)

    maybePgClassRecord <- HH.evalIO . Orville.runOrville pool $ do
      Orville.withConnection $ \connection ->
        MIO.liftIO $ TestTable.dropAndRecreateTableDef connection tableDef

      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField (String.fromString "test_pg_attrdef_query"))

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    defaults <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findEntitiesBy
        PgCatalog.pgAttributeDefaultTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.attributeDefaultRelationOidField pgClassOid)

    map PgCatalog.pgAttributeDefaultAttributeNumber defaults === [PgCatalog.attributeNumberFromInt16 1]
    map PgCatalog.pgAttributeDefaultExpression defaults === [T.pack "0"]

prop_queryPgConstraint :: Property.NamedDBProperty
prop_queryPgConstraint =
  Property.singletonNamedDBProperty "Can query the pg_constraint table to find out about a constraint" $ \pool -> do
    maybePgClassRecord <- HH.evalIO . Foo.withTable pool $ do
      Orville.findFirstEntityBy
        PgCatalog.pgClassTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField fooRelation)

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    constraints <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findEntitiesBy
        PgCatalog.pgConstraintTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.constraintRelationOidField pgClassOid)

    map PgCatalog.pgConstraintType constraints === [PgCatalog.PrimaryKeyConstraint]
    map PgCatalog.pgConstraintKey constraints === [Just [1]]

prop_queryPgTrigger :: Property.NamedDBProperty
prop_queryPgTrigger =
  Property.singletonNamedDBProperty "Can query the pg_trigger table to find out about a trigger" $ \pool -> do
    let
      triggerFunctionName =
        Expr.unqualified $ Expr.functionName "test_trigger_function"

      createTriggerFunction =
        Expr.createFunction
          (Just Expr.orReplace)
          triggerFunctionName
          (Expr.returns Expr.returnTypeTrigger)
          (Expr.language Expr.plpgsql)
          (Expr.asDefinition "BEGIN return NEW; END")

      createTrigger =
        Expr.createTrigger
          Nothing
          (Expr.triggerName "test_trigger")
          Expr.triggerBefore
          (Expr.triggerOnInsert :| [])
          (Orville.tableIdQualifiedName (Orville.tableIdentifier Foo.table))
          Expr.triggerForEachRow
          triggerFunctionName

    maybePgClassRecord <-
      HH.evalIO . Foo.withTable pool $ do
        Orville.executeVoid Orville.DDLQuery createTriggerFunction
        Orville.executeVoid Orville.DDLQuery createTrigger
        Orville.findFirstEntityBy
          PgCatalog.pgClassTable
          (Orville.where_ $ Orville.fieldEquals PgCatalog.relationNameField fooRelation)

    pgClassOid <-
      case maybePgClassRecord of
        Nothing -> do
          HH.annotate "Expected to find pg_class table, but did not"
          HH.failure
        Just pgClassRecord ->
          pure $ PgCatalog.pgClassOid pgClassRecord

    triggers <- HH.evalIO . Orville.runOrville pool $ do
      Orville.findEntitiesBy
        PgCatalog.pgTriggerTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.triggerRelationOidField pgClassOid)

    fmap PgCatalog.pgTriggerName triggers === [String.fromString "test_trigger"]

prop_queryPgProc :: Property.NamedDBProperty
prop_queryPgProc =
  Property.singletonNamedDBProperty "Can query the pg_proc table to find out about a proc" $ \pool -> do
    let
      procFunctionName =
        Expr.unqualified $ Expr.functionName "test_proc"

      procName =
        String.fromString "test_proc"

      createProcFunction =
        Expr.createFunction
          (Just Expr.orReplace)
          procFunctionName
          (Expr.returns Expr.returnTypeTrigger)
          (Expr.language Expr.plpgsql)
          (Expr.asDefinition "BEGIN return NEW; END")

    procs <- HH.evalIO . Orville.runOrville pool $ do
      Orville.executeVoid Orville.DDLQuery createProcFunction
      Orville.findEntitiesBy
        PgCatalog.pgProcTable
        (Orville.where_ $ Orville.fieldEquals PgCatalog.procNameField procName)

    fmap PgCatalog.pgProcName procs === [procName]

prop_describeDatabaseRelations :: Property.NamedDBProperty
prop_describeDatabaseRelations =
  Property.singletonNamedDBProperty "Can describe relations from different schemas at once" $ \pool -> do
    let
      relationsToDescribe =
        [ (pgCatalog, pgNamespace)
        , (pgCatalog, pgClass)
        , (informationSchema, tables)
        ]

    desc <- HH.evalIO . Orville.runOrville pool $ do
      PgCatalog.describeDatabase relationsToDescribe [] []

    Map.keysSet (PgCatalog.databaseRelations desc) === Set.fromList relationsToDescribe

    pgNamespaceDescription <-
      case Map.lookup (pgCatalog, pgNamespace) (PgCatalog.databaseRelations desc) of
        Nothing -> do
          HH.annotate "Expected to find pg_namespace table, but did not"
          HH.failure
        Just description ->
          pure description

    Map.keysSet (PgCatalog.relationAttributes pgNamespaceDescription)
      === Set.fromList
        [ String.fromString "cmax"
        , String.fromString "cmin"
        , String.fromString "ctid"
        , String.fromString "nspacl"
        , String.fromString "nspname"
        , String.fromString "nspowner"
        , String.fromString "oid"
        , String.fromString "tableoid"
        , String.fromString "xmax"
        , String.fromString "xmin"
        ]

pgCatalog :: PgCatalog.NamespaceName
pgCatalog =
  String.fromString "pg_catalog"

informationSchema :: PgCatalog.NamespaceName
informationSchema =
  String.fromString "information_schema"

pgClass :: PgCatalog.RelationName
pgClass =
  String.fromString "pg_class"

pgNamespace :: PgCatalog.RelationName
pgNamespace =
  String.fromString "pg_namespace"

tables :: PgCatalog.RelationName
tables =
  String.fromString "tables"

relname :: PgCatalog.AttributeName
relname =
  String.fromString "relname"

fooRelation :: PgCatalog.RelationName
fooRelation =
  String.fromString
    . Orville.tableIdUnqualifiedNameString
    . Orville.tableIdentifier
    $ Foo.table
