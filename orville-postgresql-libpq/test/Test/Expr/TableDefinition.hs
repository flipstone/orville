module Test.Expr.TableDefinition
  ( tableDefinitionTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.String as String

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.Expr as Expr

import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

tableDefinitionTests :: Pool.Pool Conn.Connection -> Property.Group
tableDefinitionTests pool =
  Property.group
    "Expr - TableDefinition"
    [
      ( String.fromString "Create table creates a table with one column"
      , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [column1Definition] Nothing

          PgAssert.assertColumnNamesEqual
            pool
            tableNameString
            [column1NameString]
      )
    ,
      ( String.fromString "Create table creates a table with multiple columns"
      , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [column1Definition, column2Definition] Nothing

          PgAssert.assertColumnNamesEqual
            pool
            tableNameString
            [column1NameString, column2NameString]
      )
    ,
      ( String.fromString "Alter table adds one column"
      , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [] Nothing
              Orville.executeVoid $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [])

          PgAssert.assertColumnNamesEqual
            pool
            tableNameString
            [column1NameString]
      )
    ,
      ( String.fromString "Alter table adds multiple columns"
      , Property.singletonProperty $ do
          MIO.liftIO $
            Orville.runOrville pool $ do
              Orville.executeVoid $ Expr.dropTableExpr (Just Expr.ifExists) exprTableName
              Orville.executeVoid $ Expr.createTableExpr exprTableName [] Nothing
              Orville.executeVoid $ Expr.alterTableExpr exprTableName (Expr.addColumn column1Definition :| [Expr.addColumn column2Definition])

          PgAssert.assertColumnNamesEqual
            pool
            tableNameString
            [column1NameString, column2NameString]
      )
    ]

exprTableName :: Expr.QualifiedTableName
exprTableName =
  Expr.qualifiedTableName Nothing (Expr.tableName tableNameString)

tableNameString :: String
tableNameString =
  "table_definition_test"

column1Definition :: Expr.ColumnDefinition
column1Definition =
  Expr.columnDefinition
    (Expr.columnName column1NameString)
    Expr.text
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

column2NameString :: String
column2NameString =
  "column2"
