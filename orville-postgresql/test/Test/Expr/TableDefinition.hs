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
        Orville.executeVoid Orville.DDLQuery $ Expr.createTableExpr exprTableName [column1Definition, column2Definition] Nothing []

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString, column2NameString]

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
        Orville.executeVoid Orville.DDLQuery $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [Expr.addColumn column2Definition])

    tableDesc <- PgAssert.assertTableExists pool tableNameString
    PgAssert.assertColumnNamesEqual tableDesc [column1NameString, column2NameString]

exprTableName :: Expr.Qualified Expr.TableName
exprTableName =
  Expr.qualifyTable Nothing (Expr.tableName tableNameString)

tableNameString :: String
tableNameString =
  "table_definition_test"

column1Definition :: Expr.ColumnDefinition
column1Definition =
  Expr.columnDefinition
    (Expr.columnName column1NameString)
    Expr.text
    Nothing
    Nothing

column1NameString :: String
column1NameString =
  "column1"

column2Definition :: Expr.ColumnDefinition
column2Definition =
  Expr.columnDefinition
    (Expr.columnName column2NameString)
    Expr.text
    Nothing
    Nothing

column2NameString :: String
column2NameString =
  "column2"
