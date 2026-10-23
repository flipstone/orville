module Test.ReadOnlyTableDefinition
  ( readOnlyTableDefinitionTests
  )
where

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
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Entities.Foo as Foo
import qualified Test.Entities.FooChild as FooChild
import qualified Test.Property as Property

readOnlyTableDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
readOnlyTableDefinitionTests pool =
  Tasty.testGroup
    "ReadOnlyTableDefinition"
    [ TastyHH.testProperty
        "selectTable generates an inner join of a table by name and a table by alias with prefixed result columns"
        prop_selectTable_innerJoinSql
    , TastyHH.testProperty
        "selectTable generates a left join"
        prop_selectTable_leftJoinSql
    , TastyHH.testProperty
        "joinOnCondition joins with an arbitrary condition"
        prop_selectTable_joinOnConditionSql
    , TastyHH.testProperty
        "a table with a schema is qualified by schema and table name and prefixed by both"
        prop_selectTable_schemaQualifiedSql
    , TastyHH.testProperty
        "marshallTableReferenceFields selects a subset of each table's columns"
        prop_selectTable_subsetSql
    , TastyHH.testProperty
        "tableReferenceDefinitionReadOnlyTable selects the table under its reference"
        prop_selectTable_singleTableReferenceSql
    , TastyHH.testProperty
        "findEntitiesBy returns the rows of an inner join"
        (prop_findEntitiesBy_innerJoin pool)
    , TastyHH.testProperty
        "findEntitiesBy returns Nothing for the unmatched side of a left join"
        (prop_findEntitiesBy_leftJoin pool)
    , TastyHH.testProperty
        "findEntitiesBy filters and orders a join using qualified fields"
        (prop_findEntitiesBy_whereAndOrderBy pool)
    , TastyHH.testProperty
        "mapReadOnlyTableMarshaller changes the decoded entity of a ReadOnlyTableDefinition"
        (prop_mapReadOnlyTableMarshaller pool)
    ]

prop_selectTable_innerJoinSql :: HH.Property
prop_selectTable_innerJoinSql =
  Property.singletonProperty $
    assertSelectSql
      ( selectListSql
          <> " FROM \"foo\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"foo\".\"id\")"
      )
      foosWithChildren
      Orville.emptySelectOptions

prop_selectTable_leftJoinSql :: HH.Property
prop_selectTable_leftJoinSql =
  Property.singletonProperty $
    assertSelectSql
      ( selectListSql
          <> " FROM \"foo\" LEFT JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"foo\".\"id\")"
      )
      foosWithMaybeChildren
      Orville.emptySelectOptions

prop_selectTable_joinOnConditionSql :: HH.Property
prop_selectTable_joinOnConditionSql =
  Property.singletonProperty $
    let
      byCondition =
        Orville.mkReadOnlyTableDefinition
          ( Orville.joinTables
              foo
              [ Orville.joinOnCondition Expr.innerJoinType child $
                  Expr.andExpr
                    (Expr.equals (Orville.referenceValueExpression (Orville.qualifyTableField child FooChild.fooChildFooIdField)) (Orville.referenceValueExpression (Orville.qualifyTableField foo Foo.fooIdField)))
                    (Marshall.isNotNull (Orville.qualifyTableField child FooChild.fooChildIdField))
              ]
          )
          (Orville.annotateSqlMarshallerEmptyAnnotation fooWithChildMarshaller)
    in
      assertSelectSql
        ( selectListSql
            <> " FROM \"foo\" INNER JOIN \"foo_child\" AS \"c\" ON ((\"c\".\"foo_id\") = (\"foo\".\"id\")) AND (\"c\".\"id\" IS NOT NULL)"
        )
        byCondition
        Orville.emptySelectOptions

prop_selectTable_schemaQualifiedSql :: HH.Property
prop_selectTable_schemaQualifiedSql =
  Property.singletonProperty $
    let
      fooInSchema =
        Orville.tableReferenceDefinition (Orville.setTableSchema "s" Foo.table)

      inSchema =
        Orville.mkReadOnlyTableDefinition
          ( Orville.joinTables
              fooInSchema
              [Orville.innerJoinOnFieldsEqual child FooChild.fooChildFooIdField (Orville.qualifyTableField fooInSchema Foo.fooIdField)]
          )
          ( Orville.annotateSqlMarshallerEmptyAnnotation $
              (,)
                <$> Orville.tableReferenceQualifiedMarshaller fooInSchema
                <*> Orville.tableReferenceQualifiedMarshaller child
          )
    in
      assertSelectSql
        ( "SELECT \"s\".\"foo\".\"id\" AS \"s_foo_id\",\"s\".\"foo\".\"name\" AS \"s_foo_name\",\"s\".\"foo\".\"age\" AS \"s_foo_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\""
            <> " FROM \"s\".\"foo\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"s\".\"foo\".\"id\")"
        )
        inSchema
        Orville.emptySelectOptions

prop_selectTable_subsetSql :: HH.Property
prop_selectTable_subsetSql =
  Property.singletonProperty $
    let
      namesAndChildIds :: Orville.ReadOnlyTableDefinition (Foo.FooName, Int.Int32)
      namesAndChildIds =
        Orville.mkReadOnlyTableDefinition
          foosJoinChildren
          ( Orville.annotateSqlMarshallerEmptyAnnotation $
              (,)
                <$> Orville.marshallTableReferenceFields foo (Orville.marshallReadOnlyField Foo.fooNameField)
                <*> Orville.marshallTableReferenceFields child (Orville.marshallReadOnlyField FooChild.fooChildIdField)
          )
    in
      assertSelectSql
        "SELECT \"foo\".\"name\" AS \"foo_name\",\"c\".\"id\" AS \"c_id\" FROM \"foo\" INNER JOIN \"foo_child\" AS \"c\" ON (\"c\".\"foo_id\") = (\"foo\".\"id\")"
        namesAndChildIds
        Orville.emptySelectOptions

prop_selectTable_singleTableReferenceSql :: HH.Property
prop_selectTable_singleTableReferenceSql =
  Property.singletonProperty $
    assertSelectSql
      "SELECT \"id\",\"foo_id\" FROM \"foo_child\" AS \"c\""
      (Orville.tableReferenceDefinitionReadOnlyTable child)
      Orville.emptySelectOptions

prop_findEntitiesBy_innerJoin :: Orville.ConnectionPool -> HH.Property
prop_findEntitiesBy_innerJoin pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)

    actual <-
      FooChild.withTables pool $ do
        insertFoosAndChildren foos children
        Execution.findSelectableEntitiesBy foosWithChildren Orville.emptySelectOptions

    let
      expected =
        foldMap
          (\fooChild -> fmap (\parent -> (parent, fooChild)) (filter (\parent -> FooChild.isChildOf parent fooChild) foos))
          children

    sortPairs actual === sortPairs expected

prop_findEntitiesBy_leftJoin :: Orville.ConnectionPool -> HH.Property
prop_findEntitiesBy_leftJoin pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)

    actual <-
      FooChild.withTables pool $ do
        insertFoosAndChildren foos children
        Execution.findSelectableEntitiesBy foosWithMaybeChildren Orville.emptySelectOptions

    let
      childrenOf parent =
        filter (FooChild.isChildOf parent) children

      expectedFor parent =
        case childrenOf parent of
          [] -> [(parent, Nothing)]
          fooChildren -> fmap (\fooChild -> (parent, Just fooChild)) fooChildren

      expected =
        foldMap expectedFor foos

    sortMaybePairs actual === sortMaybePairs expected

prop_findEntitiesBy_whereAndOrderBy :: Orville.ConnectionPool -> HH.Property
prop_findEntitiesBy_whereAndOrderBy pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)
    chosenFoo <- HH.forAll (Gen.element foos)

    actual <-
      FooChild.withTables pool $ do
        insertFoosAndChildren foos children
        Execution.findSelectableEntitiesBy foosWithChildren $
          Orville.where_ (Marshall.equals (Orville.qualifyTableField foo Foo.fooIdField) (Foo.fooId chosenFoo))
            <> Orville.orderBy (Orville.orderBySqlComparable (Orville.qualifyTableField child FooChild.fooChildIdField) Orville.descendingOrder)

    let
      expected =
        List.sortOn (negate . FooChild.fooChildId . snd) $
          fmap (\fooChild -> (chosenFoo, fooChild)) (filter (FooChild.isChildOf chosenFoo) children)

    actual === expected

prop_mapReadOnlyTableMarshaller :: Orville.ConnectionPool -> HH.Property
prop_mapReadOnlyTableMarshaller pool =
  HH.property $ do
    foos <- HH.forAll (Foo.generateList (Range.linear 1 5))
    children <- HH.forAll (FooChild.generateList (Range.linear 0 10) foos)

    (pairs, childrenOnly) <-
      FooChild.withTables pool $ do
        insertFoosAndChildren foos children
        (,)
          <$> Execution.findSelectableEntitiesBy foosWithChildren Orville.emptySelectOptions
          <*> Execution.findSelectableEntitiesBy (Orville.mapReadOnlyTableMarshaller (fmap snd) foosWithChildren) Orville.emptySelectOptions

    List.sortOn FooChild.fooChildId childrenOnly
      === List.sortOn FooChild.fooChildId (fmap snd pairs)

assertSelectSql ::
  HH.MonadTest m =>
  String ->
  Orville.ReadOnlyTableDefinition readEntity ->
  Orville.SelectOptions ->
  m ()
assertSelectSql expected readOnlyTable selectOptions =
  let
    actual =
      RawSql.toExampleBytes
        . Execution.selectToQueryExpr
        $ Execution.selectTable readOnlyTable selectOptions
  in
    actual === B8.pack expected

selectListSql :: String
selectListSql =
  "SELECT \"foo\".\"id\" AS \"foo_id\",\"foo\".\"name\" AS \"foo_name\",\"foo\".\"age\" AS \"foo_age\",\"c\".\"id\" AS \"c_id\",\"c\".\"foo_id\" AS \"c_foo_id\""

insertFoosAndChildren :: [Foo.Foo] -> [FooChild.FooChild] -> Orville.Orville ()
insertFoosAndChildren foos children = do
  traverse_ (Orville.insertEntity Foo.table) foos
  traverse_ (Orville.insertEntity FooChild.table) children

sortPairs :: [(Foo.Foo, FooChild.FooChild)] -> [(Foo.Foo, FooChild.FooChild)]
sortPairs =
  List.sortOn (\(parent, fooChild) -> (Foo.fooId parent, FooChild.fooChildId fooChild))

sortMaybePairs :: [(Foo.Foo, Maybe FooChild.FooChild)] -> [(Foo.Foo, Maybe FooChild.FooChild)]
sortMaybePairs =
  List.sortOn (\(parent, mbChild) -> (Foo.fooId parent, fmap FooChild.fooChildId mbChild))

foo :: Orville.TableReferenceDefinition Foo.Foo
foo =
  Orville.tableReferenceDefinition Foo.table

child :: Orville.TableReferenceDefinition FooChild.FooChild
child =
  Orville.aliasedTableReferenceDefinition (Orville.stringToAliasName "c") FooChild.table

foosJoinChildren :: Expr.TableReference
foosJoinChildren =
  Orville.joinTables
    foo
    [Orville.innerJoinOnFieldsEqual child FooChild.fooChildFooIdField (Orville.qualifyTableField foo Foo.fooIdField)]

fooWithChildMarshaller :: Orville.SqlMarshaller writeEntity (Foo.Foo, FooChild.FooChild)
fooWithChildMarshaller =
  (,)
    <$> Orville.tableReferenceQualifiedMarshaller foo
    <*> Orville.tableReferenceQualifiedMarshaller child

foosWithChildren :: Orville.ReadOnlyTableDefinition (Foo.Foo, FooChild.FooChild)
foosWithChildren =
  Orville.mkReadOnlyTableDefinition
    foosJoinChildren
    ( Orville.annotateSqlMarshaller
        (Orville.tableReferenceRowIdFieldNames foo <> Orville.tableReferenceRowIdFieldNames child)
        fooWithChildMarshaller
    )

foosWithMaybeChildren :: Orville.ReadOnlyTableDefinition (Foo.Foo, Maybe FooChild.FooChild)
foosWithMaybeChildren =
  Orville.mkReadOnlyTableDefinition
    ( Orville.joinTables
        foo
        [Orville.leftJoinOnFieldsEqual child FooChild.fooChildFooIdField (Orville.qualifyTableField foo Foo.fooIdField)]
    )
    ( Orville.annotateSqlMarshaller
        (Orville.tableReferenceRowIdFieldNames foo)
        ((,) <$> Orville.tableReferenceQualifiedMarshaller foo <*> Orville.marshallMaybe (Orville.tableReferenceQualifiedMarshaller child))
    )
