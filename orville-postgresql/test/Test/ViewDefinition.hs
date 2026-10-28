module Test.ViewDefinition
  ( viewDefinitionTests
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

viewDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
viewDefinitionTests pool =
  Tasty.testGroup
    "ViewDefinition"
    [ TastyHH.testProperty
        "mkCreateViewExpr creates the view from its query"
        prop_createViewSql
    , TastyHH.testProperty
        "selectTable selects the view's columns from the view"
        prop_selectTable_sql
    , TastyHH.testProperty
        "readOnlyTableToViewDefinition uses the query selectTable would run"
        prop_readOnlyTableToViewDefinition_querySql
    , TastyHH.testProperty
        "selectTable on a view of a join selects the prefixed result columns"
        prop_selectTable_joinViewSql
    , TastyHH.testProperty
        "setViewSchema qualifies the view name"
        prop_setViewSchema_sql
    , TastyHH.testProperty
        "viewReferenceDefinition joins a view by name and by alias"
        prop_viewReferenceDefinition_sql
    , TastyHH.testProperty
        "findSelectableEntitiesBy filters and orders rows of a view"
        (prop_findSelectableEntitiesBy pool)
    , TastyHH.testProperty
        "findSelectableEntitiesBy returns the rows of a view of a join"
        (prop_findSelectableEntitiesBy_joinView pool)
    , TastyHH.testProperty
        "mapViewMarshaller changes the decoded entity of a view"
        (prop_mapViewMarshaller pool)
    , TastyHH.testProperty
        "a view can be joined to a table via viewReferenceDefinition"
        (prop_joinViewToTable pool)
    ]

prop_createViewSql :: HH.Property
prop_createViewSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Orville.mkCreateViewExpr fooView Nothing)
      === B8.pack "CREATE VIEW \"foo_view\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""

prop_selectTable_sql :: HH.Property
prop_selectTable_sql =
  Property.singletonProperty $
    assertSelectSql
      "SELECT \"id\",\"name\",\"age\" FROM \"foo_view\" WHERE (\"age\") > ($1)"
      fooView
      (Orville.where_ (Orville.fieldGreaterThan Foo.fooAgeField 21))

prop_readOnlyTableToViewDefinition_querySql :: HH.Property
prop_readOnlyTableToViewDefinition_querySql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Orville.viewDefinitionQuery fooChildrenView)
      === RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable foosWithChildren Orville.emptySelectOptions))

prop_selectTable_joinViewSql :: HH.Property
prop_selectTable_joinViewSql =
  Property.singletonProperty $ do
    fmap RawSql.toExampleBytes (Orville.viewDefinitionColumnNames fooChildrenView)
      === fmap B8.pack ["\"foo_id\"", "\"foo_name\"", "\"foo_age\"", "\"c_id\"", "\"c_foo_id\""]

    assertSelectSql
      "SELECT \"foo_id\",\"foo_name\",\"foo_age\",\"c_id\",\"c_foo_id\" FROM \"foo_children_view\" ORDER BY \"c_id\" DESC"
      fooChildrenView
      (Orville.orderBy (Orville.orderByField childIdViewField Orville.descendingOrder))

prop_setViewSchema_sql :: HH.Property
prop_setViewSchema_sql =
  Property.singletonProperty $ do
    let
      inSchema =
        Orville.setViewSchema "s" fooView

    RawSql.toExampleBytes (Orville.mkCreateViewExpr inSchema (Just Expr.orReplace))
      === B8.pack "CREATE OR REPLACE VIEW \"s\".\"foo_view\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""

    assertSelectSql
      "SELECT \"id\",\"name\",\"age\" FROM \"s\".\"foo_view\""
      inSchema
      Orville.emptySelectOptions

prop_viewReferenceDefinition_sql :: HH.Property
prop_viewReferenceDefinition_sql =
  Property.singletonProperty $ do
    let
      byName =
        Orville.viewReferenceDefinition fooView

      byNameInSchema =
        Orville.viewReferenceDefinition (Orville.setViewSchema "s" fooView)

      byAlias =
        Orville.aliasedViewReferenceDefinition (Orville.stringToAliasName "v") fooView

    RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable (viewJoinedWithChildren byName) Orville.emptySelectOptions))
      === B8.pack
        "SELECT \"foo_view\".\"id\" AS \"foo_view_id\",\"foo_view\".\"name\" AS \"foo_view_name\",\"foo_view\".\"age\" AS \"foo_view_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\" FROM \"foo_view\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"foo_view\".\"id\")"

    RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable (viewJoinedWithChildren byNameInSchema) Orville.emptySelectOptions))
      === B8.pack
        "SELECT \"s\".\"foo_view\".\"id\" AS \"s_foo_view_id\",\"s\".\"foo_view\".\"name\" AS \"s_foo_view_name\",\"s\".\"foo_view\".\"age\" AS \"s_foo_view_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\" FROM \"s\".\"foo_view\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"s\".\"foo_view\".\"id\")"

    RawSql.toExampleBytes (Execution.selectToQueryExpr (Execution.selectTable (viewJoinedWithChildren byAlias) Orville.emptySelectOptions))
      === B8.pack
        "SELECT \"v\".\"id\" AS \"v_id\",\"v\".\"name\" AS \"v_name\",\"v\".\"age\" AS \"v_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\" FROM \"foo_view\" AS \"v\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"v\".\"id\")"

prop_findSelectableEntitiesBy :: Orville.ConnectionPool -> HH.Property
prop_findSelectableEntitiesBy pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 0 10))
    minimumAge <- HH.forAll Foo.generateFooAge

    actual <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        Execution.findSelectableEntitiesBy fooView $
          Orville.where_ (Orville.fieldGreaterThanOrEqualTo Foo.fooAgeField minimumAge)
            <> Orville.orderBy (Orville.orderByField Foo.fooIdField Orville.ascendingOrder)

    let
      expected =
        List.sortOn Foo.fooId (filter (\eachFoo -> Foo.fooAge eachFoo >= minimumAge) foos)

    actual === expected

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
        Execution.findSelectableEntitiesBy fooChildrenView $
          Orville.where_ (Orville.fieldEquals fooIdViewField (Foo.fooId chosenFoo))
            <> Orville.orderBy (Orville.orderByField childIdViewField Orville.descendingOrder)

    let
      expected =
        List.sortOn (negate . FooChild.fooChildId . snd) $
          fmap (\fooChild -> (chosenFoo, fooChild)) (filter (FooChild.isChildOf chosenFoo) children)

    actual === expected

prop_mapViewMarshaller :: Orville.ConnectionPool -> HH.Property
prop_mapViewMarshaller pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 0 10))

    names <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        Execution.findSelectableEntitiesBy (Orville.mapViewMarshaller (fmap Foo.fooName) fooView) Orville.emptySelectOptions

    List.sort names === List.sort (fmap Foo.fooName foos)

prop_joinViewToTable :: Orville.ConnectionPool -> HH.Property
prop_joinViewToTable pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)

    actual <-
      withFooTablesAndViews pool [testView fooView] $ do
        traverse_ (Orville.insertEntity Foo.table) foos
        traverse_ (Orville.insertEntity FooChild.table) children
        Execution.findSelectableEntitiesBy
          (viewJoinedWithChildren (Orville.aliasedViewReferenceDefinition (Orville.stringToAliasName "v") fooView))
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
  Orville.ViewDefinition readEntity ->
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
   recreate them, so views over them must be gone before the tables are
   dropped and again before a test finishes.
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

testView :: Orville.ViewDefinition readEntity -> TestView
testView viewDef =
  TestView
    { createTestView = Orville.executeVoid Orville.DDLQuery (Orville.mkCreateViewExpr viewDef Nothing)
    , dropTestView = Orville.executeVoid Orville.DDLQuery (Expr.dropViewExpr (Just Expr.ifExists) (pure (Orville.viewDefinitionName viewDef)) Nothing)
    }

sortPairs :: [(Foo.Foo, FooChild.FooChild)] -> [(Foo.Foo, FooChild.FooChild)]
sortPairs =
  List.sortOn (\(parent, fooChild) -> (Foo.fooId parent, FooChild.fooChildId fooChild))

fooView :: Orville.ViewDefinition Foo.Foo
fooView =
  Orville.mkViewDefinition
    "foo_view"
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

fooChildrenView :: Orville.ViewDefinition (Foo.Foo, FooChild.FooChild)
fooChildrenView =
  Orville.readOnlyTableToViewDefinition "foo_children_view" foosWithChildren

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
