module Test.Expr.TableDefinition
  ( tableDefinitionTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty ((:|)))
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

tableDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
tableDefinitionTests pool =
  Tasty.testGroup
    "Expr - TableDefinition"
    [ TastyHH.testProperty "Create table creates a table with one column" (prop_createWithOneColumn pool)
    , TastyHH.testProperty "Create table creates a table with multiple columns" (prop_createWithMultipleColumns pool)
    , TastyHH.testProperty "Rename table results in the new name existing and the old name not" (prop_renameTable pool)
    , TastyHH.testProperty "Alter table adds one column" (prop_addOneColumn pool)
    , TastyHH.testProperty "Alter table adds multiple columns" (prop_addMultipleColumns pool)
    , TastyHH.testProperty "dropTableExpr renders the drop behavior" prop_dropTableSql
    , TastyHH.testProperty "dropColumn and dropConstraint render the drop behavior" prop_dropColumnAndConstraintSql
    , TastyHH.testProperty "dropIndexExpr renders the drop behavior" prop_dropIndexSql
    , TastyHH.testProperty "truncateTablesExpr renders the drop behavior" prop_truncateTablesSql
    ]

prop_truncateTablesSql :: HH.Property
prop_truncateTablesSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.truncateTablesExpr (exprTableName :| [Expr.unqualified (Expr.tableName "other")]) (Just Expr.cascade))
      === B8.pack "TRUNCATE TABLE \"table_definition_test\", \"other\" CASCADE"

prop_dropTableSql :: HH.Property
prop_dropTableSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.dropTableExpr Nothing exprTableName Nothing)
      === B8.pack "DROP TABLE \"table_definition_test\""
    RawSql.toExampleBytes (Expr.dropTableExpr (Just Expr.ifExists) exprTableName (Just Expr.cascade))
      === B8.pack "DROP TABLE IF EXISTS \"table_definition_test\" CASCADE"

prop_dropColumnAndConstraintSql :: HH.Property
prop_dropColumnAndConstraintSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes
      ( Expr.alterTableExpr
          exprTableName
          (Expr.dropColumn (Expr.columnName "foo") (Just Expr.cascade) :| [Expr.dropConstraint (Expr.constraintName "bar") (Just Expr.restrict)])
      )
      === B8.pack "ALTER TABLE \"table_definition_test\" DROP COLUMN \"foo\" CASCADE, DROP CONSTRAINT \"bar\" RESTRICT"

prop_dropIndexSql :: HH.Property
prop_dropIndexSql =
  Property.singletonProperty $ do
    RawSql.toExampleBytes (Expr.dropIndexExpr (Expr.unqualified (Expr.indexName "foo_idx")) (Just Expr.restrict))
      === B8.pack "DROP INDEX \"foo_idx\" RESTRICT"

prop_createWithOneColumn :: Orville.ConnectionPool -> HH.Property
prop_createWithOneColumn pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition] Nothing []

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_createWithMultipleColumns :: Orville.ConnectionPool -> HH.Property
prop_createWithMultipleColumns pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition, column2Definition, column1DefinitionGenAlways, column1DefinitionGenByDefault] Nothing []

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString, column1AlwaysNameString, column1ByDefaultNameString, column2NameString]

prop_renameTable :: Orville.ConnectionPool -> HH.Property
prop_renameTable pool =
  Property.singletonProperty $ do
    let
      newTableNameString = "renamed_" <> tableNameString
      newTableName = Expr.unqualified $ Expr.tableName newTableNameString
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) newTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition] Nothing []
        Orville.executeVoid Orville.DDLQuery $ Expr.renameTableExpr exprTableName newTableName

    PgAssert.assertTableDoesNotExist pool tableNameString
    tableDesc <- PgAssert.assertTableExists pool newTableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_addOneColumn :: Orville.ConnectionPool -> HH.Property
prop_addOneColumn pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [] Nothing []
        Orville.executeVoid Orville.DDLQuery $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [])

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_addMultipleColumns :: Orville.ConnectionPool -> HH.Property
prop_addMultipleColumns pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [] Nothing []
        Orville.executeVoid Orville.DDLQuery $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [Expr.addColumn column1DefinitionGenAlways, Expr.addColumn column1DefinitionGenByDefault, Expr.addColumn column2Definition])

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString, column2NameString, column1AlwaysNameString, column1ByDefaultNameString]

exprTableName :: Expr.QualifiedOrUnqualified Expr.TableName
exprTableName =
  Expr.unqualified (Expr.tableName tableNameString)

tableNameString :: String
tableNameString =
  "table_definition_test"

column1Definition :: Expr.ColumnDefinition
column1Definition =
  Expr.columnDefinition
    (Expr.columnName column1NameString)
    Expr.text
    mempty
    Nothing

column1DefinitionGenByDefault :: Expr.ColumnDefinition
column1DefinitionGenByDefault =
  Expr.columnDefinition
    (Expr.columnName column1ByDefaultNameString)
    Expr.int
    [Expr.notNullConstraint, Expr.identityColumnConstraint Expr.byDefaultColumnIdentityGeneration]
    Nothing

column1DefinitionGenAlways :: Expr.ColumnDefinition
column1DefinitionGenAlways =
  Expr.columnDefinition
    (Expr.columnName column1AlwaysNameString)
    Expr.int
    [Expr.notNullConstraint, Expr.identityColumnConstraint Expr.alwaysColumnIdentityGeneration]
    Nothing

column1NameString :: String
column1NameString =
  "column1"

column1AlwaysNameString :: String
column1AlwaysNameString =
  "column1_always"

column1ByDefaultNameString :: String
column1ByDefaultNameString =
  "column1_by_default"

column2Definition :: Expr.ColumnDefinition
column2Definition =
  Expr.columnDefinition
    (Expr.columnName column2NameString)
    Expr.text
    mempty
    Nothing

column2NameString :: String
column2NameString =
  "column2"
