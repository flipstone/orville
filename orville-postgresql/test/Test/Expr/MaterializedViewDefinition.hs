module Test.Expr.MaterializedViewDefinition
  ( materializedViewDefinitionTests
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Entities.Foo as Foo
import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

materializedViewDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
materializedViewDefinitionTests pool =
  Tasty.testGroup
    "Expr - MaterializedViewDefinition"
    [ TastyHH.testProperty "createMaterializedViewExpr renders CREATE MATERIALIZED VIEW ... AS" prop_createSql
    , TastyHH.testProperty "createMaterializedViewExpr renders WITH DATA and WITH NO DATA" prop_createWithDataSql
    , TastyHH.testProperty "createMaterializedViewExpr renders every clause" prop_createEveryClauseSql
    , TastyHH.testProperty "dropMaterializedViewExpr renders DROP MATERIALIZED VIEW with names and drop behavior" prop_dropSql
    , TastyHH.testProperty "refreshMaterializedViewExpr renders REFRESH MATERIALIZED VIEW" prop_refreshSql
    , TastyHH.testProperty "refreshMaterializedViewExpr renders CONCURRENTLY and WITH [NO] DATA" prop_refreshClausesSql
    , TastyHH.testProperty "commentMaterializedViewExpr renders COMMENT ON MATERIALIZED VIEW" prop_commentSql
    , TastyHH.testProperty "alterMaterializedViewExpr renders every action in one statement" prop_alterEveryActionSql
    , TastyHH.testProperty "alterMaterializedViewColumnSetStorage renders each storage mode" prop_alterStorageModesSql
    , TastyHH.testProperty "the dedicated ALTER MATERIALIZED VIEW functions render their forms" prop_alterFormsSql
    , TastyHH.testProperty "Creates a materialized view with every clause, refreshes it and selects its rows" (prop_createRefreshAndSelect pool)
    , TastyHH.testProperty "Creates, comments on and drops a materialized view" (prop_createCommentAndDrop pool)
    , TastyHH.testProperty "Alters a materialized view's columns, cluster, options, owner and name" (prop_alterMaterializedView pool)
    , TastyHH.testProperty "Drops a materialized view and its dependents with CASCADE" (prop_dropCascade pool)
    ]

prop_createSql :: HH.Property
prop_createSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery Nothing)
      === B8.pack "CREATE MATERIALIZED VIEW \"materialized_view_definition_test\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""

prop_createWithDataSql :: HH.Property
prop_createWithDataSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery (Just Expr.withData))
      === B8.pack "CREATE MATERIALIZED VIEW \"materialized_view_definition_test\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH DATA"
    RawSql.toExampleBytes (Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery (Just Expr.withNoData))
      === B8.pack "CREATE MATERIALIZED VIEW \"materialized_view_definition_test\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH NO DATA"

prop_createEveryClauseSql :: HH.Property
prop_createEveryClauseSql =
  Property.singletonProperty $
    RawSql.toExampleBytes createWithEveryClause
      === B8.pack "CREATE MATERIALIZED VIEW IF NOT EXISTS \"materialized_view_definition_test\" (\"id\",\"name\",\"age\") USING \"heap\" WITH (\"fillfactor\" = 70, \"toast\".\"autovacuum_enabled\" = true) TABLESPACE \"pg_default\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH NO DATA"

prop_dropSql :: HH.Property
prop_dropSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.dropMaterializedViewExpr Nothing (pure exprViewName) Nothing)
      === B8.pack "DROP MATERIALIZED VIEW \"materialized_view_definition_test\""
    RawSql.toExampleBytes (Expr.dropMaterializedViewExpr (Just Expr.ifExists) (exprViewName :| [unqualifiedView "other_view"]) (Just Expr.cascade))
      === B8.pack "DROP MATERIALIZED VIEW IF EXISTS \"materialized_view_definition_test\", \"other_view\" CASCADE"
    RawSql.toExampleBytes (Expr.dropMaterializedViewExpr Nothing (pure exprViewName) (Just Expr.restrict))
      === B8.pack "DROP MATERIALIZED VIEW \"materialized_view_definition_test\" RESTRICT"

prop_refreshSql :: HH.Property
prop_refreshSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.refreshMaterializedViewExpr Nothing exprViewName Nothing)
      === B8.pack "REFRESH MATERIALIZED VIEW \"materialized_view_definition_test\""

prop_refreshClausesSql :: HH.Property
prop_refreshClausesSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.refreshMaterializedViewExpr (Just Expr.concurrently) exprViewName (Just Expr.withData))
      === B8.pack "REFRESH MATERIALIZED VIEW CONCURRENTLY \"materialized_view_definition_test\" WITH DATA"
    RawSql.toExampleBytes (Expr.refreshMaterializedViewExpr Nothing exprViewName (Just Expr.withNoData))
      === B8.pack "REFRESH MATERIALIZED VIEW \"materialized_view_definition_test\" WITH NO DATA"

prop_commentSql :: HH.Property
prop_commentSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.commentMaterializedViewExpr exprViewName (Just (Expr.commentText (T.pack "A materialized view"))))
      === B8.pack "COMMENT ON MATERIALIZED VIEW \"materialized_view_definition_test\" IS 'A materialized view'"

prop_alterEveryActionSql :: HH.Property
prop_alterEveryActionSql =
  Property.singletonProperty $
    RawSql.toExampleBytes
      ( Expr.alterMaterializedViewExpr
          (Just Expr.ifExists)
          exprViewName
          ( Expr.alterMaterializedViewColumnSetStatistics idColumn 100
              :| [ Expr.alterMaterializedViewColumnSetOptions idColumn (pure (Expr.relationOption (Expr.relationOptionName "n_distinct") (Just (Expr.relationOptionInt 10))))
                 , Expr.alterMaterializedViewColumnResetOptions idColumn (pure (Expr.relationOptionName "n_distinct"))
                 , Expr.alterMaterializedViewColumnSetStorage nameColumn Expr.extendedStorage
                 , Expr.alterMaterializedViewColumnSetCompression nameColumn (Expr.compressionMethod "lz4")
                 , Expr.alterMaterializedViewClusterOn testIndexName
                 , Expr.alterMaterializedViewSetWithoutCluster
                 , Expr.alterMaterializedViewSetAccessMethod (Expr.accessMethodName "heap")
                 , Expr.alterMaterializedViewSetTablespace (Expr.tablespaceName "pg_default")
                 , Expr.alterMaterializedViewSetOptions (Expr.relationOption (Expr.relationOptionName "fillfactor") (Just (Expr.relationOptionInt 70)) :| [Expr.relationOption (Expr.relationOptionName "autovacuum_enabled") (Just (Expr.relationOptionText (T.pack "off")))])
                 , Expr.alterMaterializedViewResetOptions (Expr.relationOptionName "fillfactor" :| [Expr.relationOptionName "autovacuum_enabled"])
                 , Expr.alterMaterializedViewOwnerTo (Expr.roleSpecification (Expr.roleName "some_role"))
                 ]
          )
      )
      === B8.pack
        ( "ALTER MATERIALIZED VIEW IF EXISTS \"materialized_view_definition_test\" "
            <> "ALTER COLUMN \"id\" SET STATISTICS 100, "
            <> "ALTER COLUMN \"id\" SET (\"n_distinct\" = 10), "
            <> "ALTER COLUMN \"id\" RESET (\"n_distinct\"), "
            <> "ALTER COLUMN \"name\" SET STORAGE EXTENDED, "
            <> "ALTER COLUMN \"name\" SET COMPRESSION \"lz4\", "
            <> "CLUSTER ON \"materialized_view_definition_test_id_idx\", "
            <> "SET WITHOUT CLUSTER, "
            <> "SET ACCESS METHOD \"heap\", "
            <> "SET TABLESPACE \"pg_default\", "
            <> "SET (\"fillfactor\" = 70, \"autovacuum_enabled\" = 'off'), "
            <> "RESET (\"fillfactor\", \"autovacuum_enabled\"), "
            <> "OWNER TO \"some_role\""
        )

prop_alterStorageModesSql :: HH.Property
prop_alterStorageModesSql =
  Property.singletonProperty $ do
    let
      actionSql action =
        RawSql.toExampleBytes (Expr.alterMaterializedViewExpr Nothing exprViewName (pure action))

    actionSql (Expr.alterMaterializedViewColumnSetStorage nameColumn Expr.plainStorage)
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" ALTER COLUMN \"name\" SET STORAGE PLAIN"
    actionSql (Expr.alterMaterializedViewColumnSetStorage nameColumn Expr.externalStorage)
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" ALTER COLUMN \"name\" SET STORAGE EXTERNAL"
    actionSql (Expr.alterMaterializedViewColumnSetStorage nameColumn Expr.mainStorage)
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" ALTER COLUMN \"name\" SET STORAGE MAIN"
    actionSql (Expr.alterMaterializedViewColumnSetCompression nameColumn Expr.defaultCompression)
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" ALTER COLUMN \"name\" SET COMPRESSION DEFAULT"

prop_alterFormsSql :: HH.Property
prop_alterFormsSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.renameMaterializedViewColumnExpr Nothing exprViewName (Expr.columnName "age") (Expr.columnName "years"))
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" RENAME COLUMN \"age\" TO \"years\""
    RawSql.toExampleBytes (Expr.renameMaterializedViewExpr (Just Expr.ifExists) exprViewName (Expr.tableName "renamed_view"))
      === B8.pack "ALTER MATERIALIZED VIEW IF EXISTS \"materialized_view_definition_test\" RENAME TO \"renamed_view\""
    RawSql.toExampleBytes (Expr.setMaterializedViewSchemaExpr Nothing exprViewName (Expr.schemaName "other_schema"))
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" SET SCHEMA \"other_schema\""
    RawSql.toExampleBytes (Expr.alterMaterializedViewExtensionDependencyExpr exprViewName (Expr.dependsOnExtension (Expr.extensionName "some_extension")))
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" DEPENDS ON EXTENSION \"some_extension\""
    RawSql.toExampleBytes (Expr.alterMaterializedViewExtensionDependencyExpr exprViewName (Expr.noDependsOnExtension (Expr.extensionName "some_extension")))
      === B8.pack "ALTER MATERIALIZED VIEW \"materialized_view_definition_test\" NO DEPENDS ON EXTENSION \"some_extension\""
    RawSql.toExampleBytes
      ( Expr.alterAllMaterializedViewsInTablespaceExpr
          (Expr.tablespaceName "old_space")
          [Expr.roleSpecification (Expr.roleName "some_role"), Expr.currentUser]
          (Expr.tablespaceName "new_space")
          (Just Expr.noWait)
      )
      === B8.pack "ALTER MATERIALIZED VIEW ALL IN TABLESPACE \"old_space\" OWNED BY \"some_role\", CURRENT_USER SET TABLESPACE \"new_space\" NOWAIT"
    RawSql.toExampleBytes
      (Expr.alterAllMaterializedViewsInTablespaceExpr (Expr.tablespaceName "old_space") [] (Expr.tablespaceName "new_space") Nothing)
      === B8.pack "ALTER MATERIALIZED VIEW ALL IN TABLESPACE \"old_space\" SET TABLESPACE \"new_space\""

prop_createRefreshAndSelect :: Orville.ConnectionPool -> HH.Property
prop_createRefreshAndSelect pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 0 10))

    (mbView, rowsAfterRefresh) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery createWithEveryClause
        traverse_ (Orville.insertEntity Foo.table) foos
        Orville.executeVoid Orville.OtherQuery $ Expr.refreshMaterializedViewExpr Nothing exprViewName (Just Expr.withData)
        mbView <- describeTestView viewNameString
        rows <-
          Orville.executeAndDecode
            Orville.SelectQuery
            (Execution.selectToQueryExpr (Execution.selectMarshalledColumns (Orville.tableMarshaller Foo.table) exprViewName Orville.emptySelectOptions))
            (Orville.tableMarshaller Foo.table)
        pure (mbView, rows)

    relation <- assertViewFound mbView
    PgAssert.assertColumnNamesEqual relation ["id", "name", "age"]
    List.sortOn Foo.fooId rowsAfterRefresh === List.sortOn Foo.fooId foos

prop_createCommentAndDrop :: Orville.ConnectionPool -> HH.Property
prop_createCommentAndDrop pool =
  Property.singletonProperty $ do
    let
      comment =
        T.pack "A materialized view of foo"

    (mbCreatedView, mbDroppedView) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery (Just Expr.withData)
        Orville.executeVoid Orville.DDLQuery $ Expr.commentMaterializedViewExpr exprViewName (Just (Expr.commentText comment))
        mbCreatedView <- describeTestView viewNameString
        Orville.executeVoid Orville.DDLQuery $ Expr.dropMaterializedViewExpr Nothing (pure exprViewName) Nothing
        mbDroppedView <- describeTestView viewNameString
        pure (mbCreatedView, mbDroppedView)

    relation <- assertViewFound mbCreatedView
    PgAssert.assertColumnNamesEqual relation ["id", "name", "age"]
    PgCatalog.relationComment relation === Just comment
    HH.assert (Maybe.isNothing mbDroppedView)

prop_alterMaterializedView :: Orville.ConnectionPool -> HH.Property
prop_alterMaterializedView pool =
  Property.singletonProperty $ do
    let
      alterView =
        Orville.executeVoid Orville.DDLQuery . Expr.alterMaterializedViewExpr Nothing exprViewName . pure

    (mbAltered, mbRenamed) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery (Just Expr.withData)
        Orville.executeVoid Orville.DDLQuery $
          Expr.createNamedIndexExpr Expr.UniqueIndex Nothing exprViewName testIndexName (Expr.indexBodyColumns (pure idColumn))
        alterView $ Expr.alterMaterializedViewColumnSetStatistics idColumn 100
        alterView $ Expr.alterMaterializedViewColumnSetOptions idColumn (pure (Expr.relationOption (Expr.relationOptionName "n_distinct") (Just (Expr.relationOptionInt 10))))
        alterView $ Expr.alterMaterializedViewColumnResetOptions idColumn (pure (Expr.relationOptionName "n_distinct"))
        alterView $ Expr.alterMaterializedViewColumnSetStorage nameColumn Expr.mainStorage
        alterView $ Expr.alterMaterializedViewColumnSetCompression nameColumn (Expr.compressionMethod "pglz")
        alterView $ Expr.alterMaterializedViewClusterOn testIndexName
        alterView $ Expr.alterMaterializedViewSetOptions (pure (Expr.relationOption (Expr.relationOptionName "fillfactor") (Just (Expr.relationOptionInt 70))))
        alterView $ Expr.alterMaterializedViewResetOptions (pure (Expr.relationOptionName "fillfactor"))
        alterView $ Expr.alterMaterializedViewSetTablespace (Expr.tablespaceName "pg_default")
        alterView $ Expr.alterMaterializedViewOwnerTo Expr.currentUser
        alterView Expr.alterMaterializedViewSetWithoutCluster
        Orville.executeVoid Orville.DDLQuery $ Expr.renameMaterializedViewColumnExpr Nothing exprViewName (Expr.columnName "age") (Expr.columnName "years")
        mbAltered <- describeTestView viewNameString
        Orville.executeVoid Orville.DDLQuery $ Expr.renameMaterializedViewExpr Nothing exprViewName (Expr.tableName renamedViewNameString)
        mbRenamed <- describeTestView renamedViewNameString
        pure (mbAltered, mbRenamed)

    altered <- assertViewFound mbAltered
    PgAssert.assertColumnNamesEqual altered ["id", "name", "years"]

    renamed <- assertViewFound mbRenamed
    PgAssert.assertColumnNamesEqual renamed ["id", "name", "years"]

prop_dropCascade :: Orville.ConnectionPool -> HH.Property
prop_dropCascade pool =
  Property.singletonProperty $ do
    let
      dependentQuery =
        Execution.selectToQueryExpr $
          Execution.selectMarshalledColumns (Orville.tableMarshaller Foo.table) exprViewName Orville.emptySelectOptions

    (mbView, mbDependent) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createMaterializedViewExpr Nothing exprViewName Nothing Nothing [] Nothing fooQuery (Just Expr.withData)
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprDependentViewName Nothing [] dependentQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.dropMaterializedViewExpr Nothing (pure exprViewName) (Just Expr.cascade)
        (,) <$> describeTestView viewNameString <*> describeDependentView

    HH.assert (Maybe.isNothing mbView)
    HH.assert (Maybe.isNothing mbDependent)

createWithEveryClause :: Expr.CreateMaterializedViewExpr
createWithEveryClause =
  Expr.createMaterializedViewExpr
    (Just Expr.ifNotExists)
    exprViewName
    (Just (idColumn :| [nameColumn, Expr.columnName "age"]))
    (Just (Expr.accessMethodName "heap"))
    [ Expr.relationOption (Expr.relationOptionName "fillfactor") (Just (Expr.relationOptionInt 70))
    , Expr.relationOption (Expr.toastRelationOptionName "autovacuum_enabled") (Just (Expr.relationOptionBool True))
    ]
    (Just (Expr.tablespaceName "pg_default"))
    fooQuery
    (Just Expr.withNoData)

assertViewFound :: Maybe PgCatalog.RelationDescription -> HH.PropertyT IO PgCatalog.RelationDescription
assertViewFound mbView =
  case mbView of
    Nothing -> do
      HH.annotate "Expected the materialized view to exist, but it was not found"
      HH.failure
    Just relation ->
      pure relation

{- The foo table is shared with other tests, which drop and recreate it, so
   materialized views over it must be gone before the table is dropped and
   again when a test finishes, whether or not it succeeded.
-}
withFooTableAndNoViews :: Orville.ConnectionPool -> Orville.Orville a -> HH.PropertyT IO a
withFooTableAndNoViews pool operation =
  let
    dropViews =
      Orville.runOrville pool dropTestViews
  in
    HH.evalIO $
      ExSafe.finally
        ( do
            dropViews
            Foo.withTable pool operation
        )
        dropViews

dropTestViews :: Orville.Orville ()
dropTestViews = do
  Orville.executeVoid Orville.DDLQuery $ Expr.dropViewExpr (Just Expr.ifExists) (pure exprDependentViewName) (Just Expr.cascade)
  traverse_
    (\name -> Orville.executeVoid Orville.DDLQuery $ Expr.dropMaterializedViewExpr (Just Expr.ifExists) (pure (unqualifiedView name)) (Just Expr.cascade))
    [viewNameString, renamedViewNameString]

describeTestView :: String -> Orville.Orville (Maybe PgCatalog.RelationDescription)
describeTestView =
  describeRelation PgCatalog.MaterializedView

describeDependentView :: Orville.Orville (Maybe PgCatalog.RelationDescription)
describeDependentView =
  describeRelation PgCatalog.View dependentViewNameString

describeRelation :: PgCatalog.RelationKind -> String -> Orville.Orville (Maybe PgCatalog.RelationDescription)
describeRelation kind relationName =
  let
    catalogNames =
      (String.fromString "public", String.fromString relationName)
  in
    fmap (PgCatalog.lookupRelationOfKind kind catalogNames) $
      PgCatalog.describeDatabase [catalogNames] [] []

fooQuery :: Expr.QueryExpr
fooQuery =
  Execution.selectToQueryExpr $
    Execution.selectTable Foo.table Orville.emptySelectOptions

idColumn :: Expr.ColumnName
idColumn =
  Expr.columnName "id"

nameColumn :: Expr.ColumnName
nameColumn =
  Expr.columnName "name"

testIndexName :: Expr.IndexName
testIndexName =
  Expr.indexName "materialized_view_definition_test_id_idx"

unqualifiedView :: String -> Expr.QualifiedOrUnqualified Expr.TableName
unqualifiedView =
  Expr.unqualified . Expr.tableName

exprViewName :: Expr.QualifiedOrUnqualified Expr.TableName
exprViewName =
  unqualifiedView viewNameString

exprDependentViewName :: Expr.QualifiedOrUnqualified Expr.TableName
exprDependentViewName =
  unqualifiedView dependentViewNameString

viewNameString :: String
viewNameString =
  "materialized_view_definition_test"

renamedViewNameString :: String
renamedViewNameString =
  "materialized_view_definition_test_renamed"

dependentViewNameString :: String
dependentViewNameString =
  "materialized_view_definition_test_dependent"
