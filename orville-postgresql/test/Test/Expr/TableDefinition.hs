module Test.Expr.TableDefinition
  ( tableDefinitionTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

tableDefinitionTests :: Orville.ConnectionPool -> Property.Group
tableDefinitionTests pool =
  Property.group
    "Expr - TableDefinition"
    [ prop_createWithOneColumn pool
    , prop_createWithMultipleColumns pool
    , prop_renameTable pool
    , prop_addOneColumn pool
    , prop_addMultipleColumns pool
    ]

prop_createWithOneColumn :: Property.NamedDBProperty
prop_createWithOneColumn =
  Property.singletonNamedDBProperty "Create table creates a table with one column" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition] Nothing []

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_createWithMultipleColumns :: Property.NamedDBProperty
prop_createWithMultipleColumns =
  Property.singletonNamedDBProperty "Create table creates a table with multiple columns" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition, column2Definition, column1DefinitionGenAlways, column1DefinitionGenByDefault] Nothing []

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString, column1AlwaysNameString, column1ByDefaultNameString, column2NameString]

prop_renameTable :: Property.NamedDBProperty
prop_renameTable =
  Property.singletonNamedDBProperty "Rename table results in the new name existing and the old name not" $ \pool -> do
    let
      newTableNameString = "renamed_" <> tableNameString
      newTableName = Expr.unqualified $ Expr.tableName newTableNameString
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) newTableName
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition] Nothing []
        Orville.executeVoid Orville.DDLQuery $ Expr.renameTableExpr exprTableName newTableName

    PgAssert.assertTableDoesNotExist pool tableNameString
    tableDesc <- PgAssert.assertTableExists pool newTableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_addOneColumn :: Property.NamedDBProperty
prop_addOneColumn =
  Property.singletonNamedDBProperty "Alter table adds one column" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [] Nothing []
        Orville.executeVoid Orville.DDLQuery $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [])

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString]

prop_addMultipleColumns :: Property.NamedDBProperty
prop_addMultipleColumns =
  Property.singletonNamedDBProperty "Alter table adds multiple columns" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
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
