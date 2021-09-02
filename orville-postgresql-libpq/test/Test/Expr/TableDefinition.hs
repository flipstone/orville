module Test.Expr.TableDefinition
  ( tableDefinitionTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH
import           Hedgehog ((===))

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.InformationSchema as IS
import qualified Orville.PostgreSQL.Internal.Expr as Expr

import qualified Test.Property as Property

tableDefinitionTests :: Pool.Pool Conn.Connection -> IO Bool
tableDefinitionTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "Expr - TableDefinition")
      [
        ( String.fromString "Create table creates a table with one column"
        , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [column1Definition] Nothing

          assertColumnNamesEqual
            pool
            informationSchemaTableName
            [informationSchemaColumn1Name]
        )

      , ( String.fromString "Create table creates a table with multiple columns"
        , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [column1Definition, column2Definition] Nothing

          assertColumnNamesEqual
            pool
            informationSchemaTableName
            [informationSchemaColumn1Name, informationSchemaColumn2Name]
        )

      , ( String.fromString "Alter table adds one column"
        , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [] Nothing
              Orville.executeVoid $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [])

          assertColumnNamesEqual
            pool
            informationSchemaTableName
            [informationSchemaColumn1Name]
        )

      , ( String.fromString "Alter table adds multiple columns"
        , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [] Nothing
              Orville.executeVoid $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [Expr.addColumn column2Definition])

          assertColumnNamesEqual
            pool
            informationSchemaTableName
            [informationSchemaColumn1Name, informationSchemaColumn2Name]
        )
      ]

exprTableName :: Expr.QualifiedTableName
exprTableName =
  Expr.qualifiedTableName Nothing (Expr.tableName "table_definition_test")

informationSchemaTableName :: IS.TableName
informationSchemaTableName =
  String.fromString "table_definition_test"

column1Definition :: Expr.ColumnDefinition
column1Definition =
  Expr.columnDefinition
    (Expr.columnName "column1")
    Expr.text
    Nothing

informationSchemaColumn1Name :: IS.ColumnName
informationSchemaColumn1Name =
  String.fromString "column1"

column2Definition :: Expr.ColumnDefinition
column2Definition =
  Expr.columnDefinition
    (Expr.columnName "column2")
    Expr.text
    Nothing

informationSchemaColumn2Name :: IS.ColumnName
informationSchemaColumn2Name =
  String.fromString "column2"

assertColumnNamesEqual :: (HH.MonadTest m, MIO.MonadIO m)
                       => Pool.Pool Conn.Connection
                       -> IS.TableName
                       -> [IS.ColumnName]
                       -> m ()
assertColumnNamesEqual pool tableName expectedColumns = do
  columns <-
    MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.findEntitiesBy
            IS.informationSchemaColumnsTable
            (Orville.where_ (Orville.fieldEquals IS.tableNameField tableName))

  map IS.columnName columns === expectedColumns

