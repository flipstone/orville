module Test.Expr.ViewDefinition
  ( viewDefinitionTests
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
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

viewDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
viewDefinitionTests pool =
  Tasty.testGroup
    "Expr - ViewDefinition"
    [ TastyHH.testProperty "createViewExpr renders CREATE VIEW ... AS" prop_createViewSql
    , TastyHH.testProperty "createViewExpr renders every clause" prop_createViewEveryClauseSql
    , TastyHH.testProperty "checkOption renders each level" prop_checkOptionSql
    , TastyHH.testProperty "createViewExpr renders a generic option without a value" prop_createViewGenericOptionSql
    , TastyHH.testProperty "dropViewExpr renders DROP VIEW with names and drop behavior" prop_dropViewSql
    , TastyHH.testProperty "alterViewExpr renders each action" prop_alterViewActionsSql
    , TastyHH.testProperty "alterViewOwnerTo renders each role specification" prop_alterViewOwnerSql
    , TastyHH.testProperty "the dedicated ALTER VIEW functions render their forms" prop_alterViewFormsSql
    , TastyHH.testProperty "commentViewExpr renders COMMENT ON VIEW" prop_commentViewSql
    , TastyHH.testProperty "Creates a view with a column list, options and check option" (prop_createWithClauses pool)
    , TastyHH.testProperty "Creates, comments on and drops a view" (prop_createCommentAndDrop pool)
    , TastyHH.testProperty "Replaces an existing view" (prop_replaceView pool)
    , TastyHH.testProperty "Alters a view's columns, options, owner and name" (prop_alterView pool)
    , TastyHH.testProperty "Drops a view and its dependents with CASCADE" (prop_dropCascade pool)
    ]

prop_createViewSql :: HH.Property
prop_createViewSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.createViewExpr Nothing Nothing Nothing exprViewName Nothing [] fooQuery Nothing)
      === B8.pack "CREATE VIEW \"view_definition_test\" AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""

prop_createViewEveryClauseSql :: HH.Property
prop_createViewEveryClauseSql =
  Property.singletonProperty $
    RawSql.toExampleBytes
      ( Expr.createViewExpr
          (Just Expr.orReplace)
          (Just Expr.temporary)
          (Just Expr.recursive)
          exprViewName
          (Just fooColumnNames)
          [ Expr.viewOptionCheckOption Expr.LocalCheckOption
          , Expr.viewOptionSecurityBarrier True
          , Expr.viewOptionSecurityInvoker False
          ]
          fooQuery
          (Just (Expr.checkOption (Just Expr.CascadedCheckOption)))
      )
      === B8.pack "CREATE OR REPLACE TEMPORARY RECURSIVE VIEW \"view_definition_test\" (\"id\",\"name\",\"age\") WITH (check_option = local, security_barrier = true, security_invoker = false) AS SELECT \"id\",\"name\",\"age\" FROM \"foo\" WITH CASCADED CHECK OPTION"

prop_checkOptionSql :: HH.Property
prop_checkOptionSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.checkOption Nothing) === B8.pack "WITH CHECK OPTION"
    RawSql.toExampleBytes (Expr.checkOption (Just Expr.LocalCheckOption)) === B8.pack "WITH LOCAL CHECK OPTION"
    RawSql.toExampleBytes (Expr.checkOption (Just Expr.CascadedCheckOption)) === B8.pack "WITH CASCADED CHECK OPTION"

prop_createViewGenericOptionSql :: HH.Property
prop_createViewGenericOptionSql =
  Property.singletonProperty $
    RawSql.toExampleBytes
      ( Expr.createViewExpr
          Nothing
          Nothing
          Nothing
          exprViewName
          Nothing
          [Expr.relationOption (Expr.relationOptionName "security_barrier") Nothing]
          fooQuery
          Nothing
      )
      === B8.pack "CREATE VIEW \"view_definition_test\" WITH (\"security_barrier\") AS SELECT \"id\",\"name\",\"age\" FROM \"foo\""

prop_dropViewSql :: HH.Property
prop_dropViewSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.dropViewExpr Nothing (pure exprViewName) Nothing)
      === B8.pack "DROP VIEW \"view_definition_test\""
    RawSql.toExampleBytes (Expr.dropViewExpr (Just Expr.ifExists) (exprViewName :| [unqualifiedView "other_view"]) (Just Expr.cascade))
      === B8.pack "DROP VIEW IF EXISTS \"view_definition_test\", \"other_view\" CASCADE"
    RawSql.toExampleBytes (Expr.dropViewExpr Nothing (pure exprViewName) (Just Expr.restrict))
      === B8.pack "DROP VIEW \"view_definition_test\" RESTRICT"

prop_alterViewActionsSql :: HH.Property
prop_alterViewActionsSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.alterViewExpr Nothing exprViewName (Expr.alterViewColumnSetDefault ageColumn (RawSql.intDecLiteral 0)))
      === B8.pack "ALTER VIEW \"view_definition_test\" ALTER COLUMN \"age\" SET DEFAULT 0"
    RawSql.toExampleBytes (Expr.alterViewExpr (Just Expr.ifExists) exprViewName (Expr.alterViewColumnDropDefault ageColumn))
      === B8.pack "ALTER VIEW IF EXISTS \"view_definition_test\" ALTER COLUMN \"age\" DROP DEFAULT"
    RawSql.toExampleBytes
      ( Expr.alterViewExpr Nothing exprViewName $
          Expr.alterViewSetOptions (Expr.viewOptionCheckOption Expr.CascadedCheckOption :| [Expr.viewOptionSecurityBarrier False])
      )
      === B8.pack "ALTER VIEW \"view_definition_test\" SET (check_option = cascaded, security_barrier = false)"
    RawSql.toExampleBytes
      ( Expr.alterViewExpr Nothing exprViewName $
          Expr.alterViewResetOptions (Expr.viewOptionNameCheckOption :| [Expr.viewOptionNameSecurityBarrier])
      )
      === B8.pack "ALTER VIEW \"view_definition_test\" RESET (check_option, security_barrier)"

prop_alterViewOwnerSql :: HH.Property
prop_alterViewOwnerSql =
  Property.singletonProperty $ do
    let
      ownerSql role =
        RawSql.toExampleBytes (Expr.alterViewExpr Nothing exprViewName (Expr.alterViewOwnerTo role))

    ownerSql (Expr.roleSpecification (Expr.roleName "some_role")) === B8.pack "ALTER VIEW \"view_definition_test\" OWNER TO \"some_role\""
    ownerSql Expr.currentRole === B8.pack "ALTER VIEW \"view_definition_test\" OWNER TO CURRENT_ROLE"
    ownerSql Expr.currentUser === B8.pack "ALTER VIEW \"view_definition_test\" OWNER TO CURRENT_USER"
    ownerSql Expr.sessionUser === B8.pack "ALTER VIEW \"view_definition_test\" OWNER TO SESSION_USER"

prop_alterViewFormsSql :: HH.Property
prop_alterViewFormsSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.renameViewColumnExpr Nothing exprViewName ageColumn (Expr.columnName "years"))
      === B8.pack "ALTER VIEW \"view_definition_test\" RENAME COLUMN \"age\" TO \"years\""
    RawSql.toExampleBytes (Expr.renameViewExpr (Just Expr.ifExists) exprViewName (Expr.tableName "renamed_view"))
      === B8.pack "ALTER VIEW IF EXISTS \"view_definition_test\" RENAME TO \"renamed_view\""
    RawSql.toExampleBytes (Expr.setViewSchemaExpr Nothing exprViewName (Expr.schemaName "other_schema"))
      === B8.pack "ALTER VIEW \"view_definition_test\" SET SCHEMA \"other_schema\""

prop_commentViewSql :: HH.Property
prop_commentViewSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.commentViewExpr exprViewName (Just (Expr.commentText (T.pack "A view"))))
      === B8.pack "COMMENT ON VIEW \"view_definition_test\" IS 'A view'"

prop_createWithClauses :: Orville.ConnectionPool -> HH.Property
prop_createWithClauses pool =
  Property.singletonProperty $ do
    mbView <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $
          Expr.createViewExpr
            Nothing
            Nothing
            Nothing
            exprViewName
            (Just (Expr.columnName "foo_id" :| [Expr.columnName "foo_name", Expr.columnName "foo_age"]))
            [Expr.viewOptionSecurityBarrier True]
            fooQuery
            (Just (Expr.checkOption Nothing))
        describeTestView viewNameString

    relation <- assertViewFound mbView
    PgAssert.assertColumnNamesEqual relation ["foo_id", "foo_name", "foo_age"]

prop_createCommentAndDrop :: Orville.ConnectionPool -> HH.Property
prop_createCommentAndDrop pool =
  Property.singletonProperty $ do
    let
      comment =
        T.pack "A view of foo"

    (mbCreatedView, mbDroppedView) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprViewName Nothing [] fooQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.commentViewExpr exprViewName (Just (Expr.commentText comment))
        mbCreatedView <- describeTestView viewNameString
        Orville.executeVoid Orville.DDLQuery $ Expr.dropViewExpr Nothing (pure exprViewName) Nothing
        mbDroppedView <- describeTestView viewNameString
        pure (mbCreatedView, mbDroppedView)

    relation <- assertViewFound mbCreatedView
    PgAssert.assertColumnNamesEqual relation ["id", "name", "age"]
    PgCatalog.relationComment relation === Just comment
    HH.assert (Maybe.isNothing mbDroppedView)

prop_replaceView :: Orville.ConnectionPool -> HH.Property
prop_replaceView pool =
  Property.singletonProperty $ do
    let
      idOnlyQuery =
        Execution.selectToQueryExpr $
          Execution.selectMarshalledColumns
            (Orville.annotateSqlMarshallerEmptyAnnotation (Orville.marshallField id Foo.fooIdField))
            (Orville.tableName Foo.table)
            Orville.emptySelectOptions

    mbView <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprViewName Nothing [] idOnlyQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr (Just Expr.orReplace) Nothing Nothing exprViewName Nothing [] fooQuery Nothing
        describeTestView viewNameString

    relation <- assertViewFound mbView
    PgAssert.assertColumnNamesEqual relation ["id", "name", "age"]

prop_alterView :: Orville.ConnectionPool -> HH.Property
prop_alterView pool =
  Property.singletonProperty $ do
    let
      alterView =
        Orville.executeVoid Orville.DDLQuery . Expr.alterViewExpr Nothing exprViewName

      yearsColumn =
        Expr.columnName "years"

      yearsDefault relation = do
        attribute <- PgCatalog.lookupAttribute (String.fromString "years") relation
        PgCatalog.lookupAttributeDefault attribute relation

    (mbWithDefault, mbWithoutDefault, mbRenamed) <-
      withFooTableAndNoViews pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprViewName Nothing [] fooQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.renameViewColumnExpr Nothing exprViewName ageColumn yearsColumn
        alterView $ Expr.alterViewColumnSetDefault yearsColumn (RawSql.intDecLiteral 0)
        mbWithDefault <- describeTestView viewNameString
        alterView $ Expr.alterViewColumnDropDefault yearsColumn
        mbWithoutDefault <- describeTestView viewNameString
        alterView $ Expr.alterViewOwnerTo Expr.currentUser
        alterView $ Expr.alterViewSetOptions (Expr.viewOptionSecurityBarrier True :| [Expr.viewOptionCheckOption Expr.LocalCheckOption])
        alterView $ Expr.alterViewResetOptions (Expr.viewOptionNameSecurityBarrier :| [Expr.viewOptionNameCheckOption])
        Orville.executeVoid Orville.DDLQuery $ Expr.renameViewExpr Nothing exprViewName (Expr.tableName renamedViewNameString)
        mbRenamed <- describeTestView renamedViewNameString
        pure (mbWithDefault, mbWithoutDefault, mbRenamed)

    withDefault <- assertViewFound mbWithDefault
    PgAssert.assertColumnNamesEqual withDefault ["id", "name", "years"]
    fmap PgCatalog.pgAttributeDefaultExpression (yearsDefault withDefault) === Just (T.pack "0")

    withoutDefault <- assertViewFound mbWithoutDefault
    HH.assert (Maybe.isNothing (yearsDefault withoutDefault))

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
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprViewName Nothing [] fooQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createViewExpr Nothing Nothing Nothing exprDependentViewName Nothing [] dependentQuery Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.dropViewExpr Nothing (pure exprViewName) (Just Expr.cascade)
        (,) <$> describeTestView viewNameString <*> describeTestView dependentViewNameString

    HH.assert (Maybe.isNothing mbView)
    HH.assert (Maybe.isNothing mbDependent)

assertViewFound :: Maybe PgCatalog.RelationDescription -> HH.PropertyT IO PgCatalog.RelationDescription
assertViewFound mbView =
  case mbView of
    Nothing -> do
      HH.annotate "Expected the view to exist, but it was not found"
      HH.failure
    Just relation ->
      pure relation

{- The foo table is shared with other tests, which drop and recreate it, so
   views over it must be gone before the table is dropped and again when a
   test finishes, whether or not it succeeded.
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
dropTestViews =
  traverse_
    (\name -> Orville.executeVoid Orville.DDLQuery $ Expr.dropViewExpr (Just Expr.ifExists) (pure (unqualifiedView name)) (Just Expr.cascade))
    [dependentViewNameString, viewNameString, renamedViewNameString]

describeTestView :: String -> Orville.Orville (Maybe PgCatalog.RelationDescription)
describeTestView viewName =
  let
    catalogNames =
      (String.fromString "public", String.fromString viewName)
  in
    fmap (PgCatalog.lookupRelationOfKind PgCatalog.View catalogNames) $
      PgCatalog.describeDatabase [catalogNames] [] []

fooQuery :: Expr.QueryExpr
fooQuery =
  Execution.selectToQueryExpr $
    Execution.selectTable Foo.table Orville.emptySelectOptions

fooColumnNames :: NonEmpty Expr.ColumnName
fooColumnNames =
  Expr.columnName "id" :| [Expr.columnName "name", ageColumn]

ageColumn :: Expr.ColumnName
ageColumn =
  Expr.columnName "age"

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
  "view_definition_test"

renamedViewNameString :: String
renamedViewNameString =
  "view_definition_test_renamed"

dependentViewNameString :: String
dependentViewNameString =
  "view_definition_test_dependent"
