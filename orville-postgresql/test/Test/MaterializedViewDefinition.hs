module Test.MaterializedViewDefinition
  ( materializedViewDefinitionTests
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (traverse_)
import qualified Data.Int as Int
import qualified Data.List as List
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Entities.Foo as Foo
import qualified Test.Entities.FooChild as FooChild
import qualified Test.Property as Property

materializedViewDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
materializedViewDefinitionTests pool =
  Tasty.testGroup
    "MaterializedViewDefinition"
    [ TastyHH.testProperty
        "mkCreateMaterializedViewExpr creates the materialized view from its query"
        prop_createSql
    , TastyHH.testProperty
        "selectTable selects the materialized view's columns from it"
        prop_selectTable_sql
    , TastyHH.testProperty
        "readOnlyTableToMaterializedViewDefinition uses the query selectTable would run"
        prop_readOnlyTableToMaterializedViewDefinition_querySql
    , TastyHH.testProperty
        "selectTable on a materialized view of a join selects the prefixed result columns"
        prop_selectTable_joinViewSql
    , TastyHH.testProperty
        "setMaterializedViewSchema qualifies the materialized view name"
        prop_setMaterializedViewSchema_sql
    , TastyHH.testProperty
        "materializedViewReferenceDefinition joins a materialized view by name and by alias"
        prop_materializedViewReferenceDefinition_sql
    , TastyHH.testProperty
        "refreshMaterializedView makes inserted rows visible through findSelectableEntitiesBy"
        (prop_refreshMaterializedView pool)
    , TastyHH.testProperty
        "findSelectableEntitiesBy returns the rows of a materialized view of a join"
        (prop_findSelectableEntitiesBy_joinView pool)
    , TastyHH.testProperty
        "mapMaterializedViewMarshaller changes the decoded entity of a materialized view"
        (prop_mapMaterializedViewMarshaller pool)
    , TastyHH.testProperty
        "a materialized view can be joined to a table via materializedViewReferenceDefinition"
        (prop_joinMaterializedViewToTable pool)
    ]

prop_createSql :: HH.Property
prop_createSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Orville.mkCreateMaterializedViewExpr fooView Nothing)
      === B8.pack "CREATE MATERIALIZED VIEW \"foo_materialized_view\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""
    RawSql.toExampleBytes (Orville.mkCreateMaterializedViewExpr fooView (Just Expr.withNoData))
      === B8.pack "CREATE MATERIALIZED VIEW \"foo_materialized_view\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH NO DATA"

prop_selectTable_sql :: HH.Property
prop_selectTable_sql =
  Property.singletonProperty $
    assertSelectSql
      "SELECT \"id\",\"name\",\"age\" FROM \"foo_materialized_view\" WHERE (\"age\") > ($1)"
      fooView
      (Orville.where_ (Orville.fieldGreaterThan Foo.fooAgeField 21))

prop_readOnlyTableToMaterializedViewDefinition_querySql :: HH.Property
prop_readOnlyTableToMaterializedViewDefinition_querySql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Orville.materializedViewDefinitionQuery fooChildrenView)
      === RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable foosWithChildren Orville.emptySelectOptions))

prop_selectTable_joinViewSql :: HH.Property
prop_selectTable_joinViewSql =
  Property.singletonProperty $ do
    fmap RawSql.toExampleBytes (Orville.materializedViewDefinitionColumnNames fooChildrenView)
      === fmap B8.pack ["\"foo_id\"", "\"foo_name\"", "\"foo_age\"", "\"c_id\"", "\"c_foo_id\""]

    assertSelectSql
      "SELECT \"foo_id\",\"foo_name\",\"foo_age\",\"c_id\",\"c_foo_id\" FROM \"foo_children_materialized_view\" ORDER BY \"c_id\" DESC"
      fooChildrenView
      (Orville.orderBy (Orville.orderByField childIdViewField Orville.descendingOrder))

prop_setMaterializedViewSchema_sql :: HH.Property
prop_setMaterializedViewSchema_sql =
  Property.singletonProperty $ do
    let
      inSchema =
        Orville.setMaterializedViewSchema "s" fooView

    RawSql.toExampleBytes (Orville.mkCreateMaterializedViewExpr inSchema (Just Expr.withData))
      === B8.pack "CREATE MATERIALIZED VIEW \"s\".\"foo_materialized_view\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH DATA"

    assertSelectSql
      "SELECT \"id\",\"name\",\"age\" FROM \"s\".\"foo_materialized_view\""
      inSchema
      Orville.emptySelectOptions

prop_materializedViewReferenceDefinition_sql :: HH.Property
prop_materializedViewReferenceDefinition_sql =
  Property.singletonProperty $ do
    let
      byName =
        Orville.materializedViewReferenceDefinition fooView

      byAlias =
        Orville.aliasedMaterializedViewReferenceDefinition (Orville.stringToAliasName "v") fooView

    RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable (viewJoinedWithChildren byName) Orville.emptySelectOptions))
      === B8.pack
        "SELECT \"foo_materialized_view\".\"id\" AS \"foo_materialized_view_id\",\"foo_materialized_view\".\"name\" AS \"foo_materialized_view_name\",\"foo_materialized_view\".\"age\" AS \"foo_materialized_view_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\" FROM \"foo_materialized_view\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"foo_materialized_view\".\"id\")"

    RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable (viewJoinedWithChildren byAlias) Orville.emptySelectOptions))
      === B8.pack
        "SELECT \"v\".\"id\" AS \"v_id\",\"v\".\"name\" AS \"v_name\",\"v\".\"age\" AS \"v_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\" FROM \"foo_materialized_view\" AS \"v\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"v\".\"id\")"

prop_refreshMaterializedView :: Orville.ConnectionPool -> HH.Property
prop_refreshMaterializedView pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 0 10))
    minimumAge <- HH.forAll Foo.generateFooAge

    (beforeRefresh, afterRefresh) <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        beforeRefresh <- Execution.findSelectableEntitiesBy fooView Orville.emptySelectOptions
        Execution.refreshMaterializedView fooView Execution.RefreshExclusively
        afterRefresh <-
          Execution.findSelectableEntitiesBy fooView $
            Orville.where_ (Orville.fieldGreaterThanOrEqualTo Foo.fooAgeField minimumAge)
              <> Orville.orderBy (Orville.orderByField Foo.fooIdField Orville.ascendingOrder)
        pure (beforeRefresh, afterRefresh)

    beforeRefresh === []
    afterRefresh === List.sortOn Foo.fooId (filter (\eachFoo -> Foo.fooAge eachFoo >= minimumAge) foos)

prop_findSelectableEntitiesBy_joinView :: Orville.ConnectionPool -> HH.Property
prop_findSelectableEntitiesBy_joinView pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)
    chosenFoo <- HH.forAll (Gen.element foos)

    actual <-
      withFooTablesAndViews pool [testView fooChildrenView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        traverse_ (Orville.insertEntity FooChild.table) children
        Execution.refreshMaterializedView fooChildrenView Execution.RefreshExclusively
        Execution.findSelectableEntitiesBy fooChildrenView $
          Orville.where_ (Orville.fieldEquals fooIdViewField (Foo.fooId chosenFoo))
            <> Orville.orderBy (Orville.orderByField childIdViewField Orville.descendingOrder)

    let
      expected =
        List.sortOn (negate . FooChild.fooChildId . snd) $
          fmap (\fooChild -> (chosenFoo, fooChild)) (filter (FooChild.isChildOf chosenFoo) children)

    actual === expected

prop_mapMaterializedViewMarshaller :: Orville.ConnectionPool -> HH.Property
prop_mapMaterializedViewMarshaller pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 0 10))

    names <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        Execution.refreshMaterializedView fooView Execution.RefreshExclusively
        Execution.findSelectableEntitiesBy (Orville.mapMaterializedViewMarshaller (fmap Foo.fooName) fooView) Orville.emptySelectOptions

    List.sort names === List.sort (fmap Foo.fooName foos)

prop_joinMaterializedViewToTable :: Orville.ConnectionPool -> HH.Property
prop_joinMaterializedViewToTable pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)

    actual <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        traverse_ (Orville.insertEntity FooChild.table) children
        Execution.refreshMaterializedView fooView Execution.RefreshExclusively
        Execution.findSelectableEntitiesBy
          (viewJoinedWithChildren (Orville.aliasedMaterializedViewReferenceDefinition (Orville.stringToAliasName "v") fooView))
          Orville.emptySelectOptions

    let
      expected =
        foldMap
          (\fooChild -> fmap (\parent -> (parent, fooChild)) (filter (\parent -> FooChild.isChildOf parent fooChild) foos))
          children

    sortPairs actual === sortPairs expected

assertSelectSql ::
  HH.MonadTest m =>
  String ->
  Orville.MaterializedViewDefinition readEntity ->
  Orville.SelectOptions ->
  m ()
assertSelectSql expected viewDef selectOptions =
  let
    actual =
      RawSql.toExampleBytes
        . Execution.selectToQueryExpr
        $ Execution.selectTable viewDef selectOptions
  in
    actual === B8.pack expected

{- The foo and foo_child tables are shared with other tests, which drop and
   recreate them, so materialized views over them must be gone before the
   tables are dropped and again when a test finishes, whether or not it
   succeeded. The materialized views are created while the tables are empty,
   so each test refreshes them after inserting its rows.
-}
withFooTablesAndViews ::
  Orville.ConnectionPool ->
  [TestView] ->
  Orville.Orville a ->
  HH.PropertyT IO a
withFooTablesAndViews pool views operation =
  let
    dropViews =
      Orville.runOrville pool (traverse_ dropTestView views)
  in
    HH.evalIO $
      ExSafe.finally
        ( do
            dropViews
            FooChild.withTables pool (traverse_ createTestView views *> operation)
        )
        dropViews

data TestView = TestView
  { createTestView :: Orville.Orville ()
  , dropTestView :: Orville.Orville ()
  }

testView :: Orville.MaterializedViewDefinition readEntity -> TestView
testView viewDef =
  TestView
    { createTestView = Orville.executeVoid Orville.DDLQuery (Orville.mkCreateMaterializedViewExpr viewDef (Just Expr.withData))
    , dropTestView = Orville.executeVoid Orville.DDLQuery (Expr.dropMaterializedViewExpr (Just Expr.ifExists) (pure (Orville.materializedViewDefinitionName viewDef)) Nothing)
    }

sortPairs :: [(Foo.Foo, FooChild.FooChild)] -> [(Foo.Foo, FooChild.FooChild)]
sortPairs =
  List.sortOn (\(parent, fooChild) -> (Foo.fooId parent, FooChild.fooChildId fooChild))

fooView :: Orville.MaterializedViewDefinition Foo.Foo
fooView =
  Orville.mkMaterializedViewDefinition
    "foo_materialized_view"
    (Execution.selectToQueryExpr (Execution.selectTable Foo.table Orville.emptySelectOptions))
    (Orville.tableMarshaller Foo.table)

foo :: Orville.TableReferenceDefinition Foo.Foo
foo =
  Orville.tableReferenceDefinition Foo.table

child :: Orville.TableReferenceDefinition FooChild.FooChild
child =
  Orville.aliasedTableReferenceDefinition (Orville.stringToAliasName "c") FooChild.table

foosWithChildren :: Orville.ReadOnlyTableDefinition (Foo.Foo, FooChild.FooChild)
foosWithChildren =
  Orville.mkReadOnlyTableDefinition
    ( Orville.joinTables
        foo
        [Orville.innerJoinOnFieldsEqual child FooChild.fooChildFooIdField (Orville.qualifyTableField foo Foo.fooIdField)]
    )
    ( Orville.annotateSqlMarshaller
        (Orville.tableReferenceRowIdFieldNames foo <> Orville.tableReferenceRowIdFieldNames child)
        ((,) <$> Orville.tableReferenceQualifiedMarshaller foo <*> Orville.tableReferenceQualifiedMarshaller child)
    )

fooChildrenView :: Orville.MaterializedViewDefinition (Foo.Foo, FooChild.FooChild)
fooChildrenView =
  Orville.readOnlyTableToMaterializedViewDefinition "foo_children_materialized_view" foosWithChildren

fooIdViewField :: Orville.FieldDefinition Orville.NotNull Foo.FooId
fooIdViewField =
  Orville.integerField "foo_id"

childIdViewField :: Orville.FieldDefinition Orville.NotNull Int.Int32
childIdViewField =
  Orville.integerField "c_id"

viewJoinedWithChildren ::
  Orville.TableReferenceDefinition Foo.Foo ->
  Orville.ReadOnlyTableDefinition (Foo.Foo, FooChild.FooChild)
viewJoinedWithChildren viewRef =
  Orville.mkReadOnlyTableDefinition
    ( Orville.joinTables
        viewRef
        [Orville.innerJoinOnFieldsEqual child FooChild.fooChildFooIdField (Orville.qualifyTableField viewRef Foo.fooIdField)]
    )
    ( Orville.annotateSqlMarshallerEmptyAnnotation $
        (,) <$> Orville.tableReferenceQualifiedMarshaller viewRef <*> Orville.tableReferenceQualifiedMarshaller child
    )
