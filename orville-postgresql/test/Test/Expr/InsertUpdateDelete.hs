module Test.Expr.InsertUpdateDelete
  ( insertUpdateDeleteTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, findAllFooBars, findAllFooBarsInTable, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

insertUpdateDeleteTests :: Orville.ConnectionPool -> Property.Group
insertUpdateDeleteTests pool =
  Property.group
    "Expr - Insert/Update/Delete/Truncate"
    [ prop_insertExpr pool
    , prop_insertExprWithOnConflictDoNothing pool
    , prop_insertExprWithReturning pool
    , prop_updateExpr pool
    , prop_updateExprWithWhere pool
    , prop_updateExprWithReturning pool
    , prop_deleteExpr pool
    , prop_deleteExprWithWhere pool
    , prop_deleteExprWithReturning pool
    , prop_truncateTablesExpr pool
    , prop_insertExprWithOnConflictDoUpdate pool
    ]

prop_insertExpr :: Property.NamedDBProperty
prop_insertExpr =
  Property.singletonNamedDBProperty "insertExpr inserts values" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows fooBars

prop_insertExprWithOnConflictDoNothing :: Property.NamedDBProperty
prop_insertExprWithOnConflictDoNothing =
  Property.singletonNamedDBProperty "insertExpr with on conflict do nothing does not modify table" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      addIndex = RawSql.fromString "CREATE UNIQUE INDEX ON " <> RawSql.toRawSql fooBarTable <> RawSql.fromString "( foo )"

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection
          RawSql.executeVoid connection addIndex

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) (Just Expr.onConflictDoNothing) Nothing

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows fooBars

prop_insertExprWithOnConflictDoUpdate :: Property.NamedDBProperty
prop_insertExprWithOnConflictDoUpdate =
  Property.singletonNamedDBProperty "insertExpr with on conflict do update updates conflicting row" $ \pool -> do
    let
      fooBars0 = [mkFooBar 1 "dog"]
      fooBars1 = [mkFooBar 1 "eagel"]
      addIndex = RawSql.fromString "CREATE UNIQUE INDEX ON " <> RawSql.toRawSql fooBarTable <> RawSql.fromString "( foo )"
      fooColumnName = Expr.unqualified (Expr.columnName "foo")
      barColumnName = Expr.unqualified (Expr.columnName "bar")
      conflictTarget = Expr.conflictTargetForIndexColumn fooColumnName Nothing
      setExcludedColumn = Expr.setColumnNameExcluded barColumnName
      onConflictDoUpdate = Expr.onConflictDoUpdate (Just conflictTarget) (pure setExcludedColumn) Nothing

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection
          RawSql.executeVoid connection addIndex

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars0) (Just onConflictDoUpdate) Nothing

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars1) (Just onConflictDoUpdate) Nothing

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows fooBars1

prop_insertExprWithReturning :: Property.NamedDBProperty
prop_insertExprWithReturning =
  Property.singletonNamedDBProperty "insertExpr with returning clause returns the requested columns" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          result <-
            RawSql.execute connection $
              Expr.insertExpr
                fooBarTable
                Nothing
                (insertFooBarSource fooBars)
                Nothing
                (Just $ Expr.returningExpr $ Expr.selectColumns [fooColumn, barColumn])

          Execution.readRows result

    assertEqualFooBarRows rows fooBars

prop_updateExpr :: Property.NamedDBProperty
prop_updateExpr =
  Property.singletonNamedDBProperty "updateExpr updates rows in the db" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      newFooBars = [mkFooBar 1 "ferret", mkFooBar 2 "ferret"]

      setBarToFerret =
        Expr.updateExpr
          fooBarTable
          (Expr.setClauseList (Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret")) :| []))
          Nothing
          Nothing

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          RawSql.executeVoid connection setBarToFerret

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows newFooBars

prop_updateExprWithWhere :: Property.NamedDBProperty
prop_updateExprWithWhere =
  Property.singletonNamedDBProperty "updateExpr uses a where clause when given" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      newFooBars = [mkFooBar 1 "ferret", mkFooBar 2 "cat"]

      updateDogToForret =
        Expr.updateExpr
          fooBarTable
          (Expr.setClauseList (Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret")) :| []))
          (Just (Expr.whereClause (Expr.equals barColumnRef (Expr.valueExpression (SqlValue.fromText (T.pack "dog"))))))
          Nothing

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          RawSql.executeVoid connection updateDogToForret

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows newFooBars

prop_updateExprWithReturning :: Property.NamedDBProperty
prop_updateExprWithReturning =
  Property.singletonNamedDBProperty "updateExpr with returning clause returns the new records" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      newFooBars = [mkFooBar 1 "ferret", mkFooBar 2 "ferret"]

      setBarToFerret =
        Expr.updateExpr
          fooBarTable
          (Expr.setClauseList (Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret")) :| []))
          Nothing
          (Just $ Expr.returningExpr $ Expr.selectColumns [fooColumn, barColumn])

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          result <- RawSql.execute connection setBarToFerret

          Execution.readRows result

    assertEqualFooBarRows rows newFooBars

prop_deleteExpr :: Property.NamedDBProperty
prop_deleteExpr =
  Property.singletonNamedDBProperty "deleteExpr deletes rows in the db" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

      deleteRows =
        Expr.deleteExpr
          fooBarTable
          Nothing
          Nothing

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          RawSql.executeVoid connection deleteRows

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows []

prop_deleteExprWithWhere :: Property.NamedDBProperty
prop_deleteExprWithWhere =
  Property.singletonNamedDBProperty "deleteExpr uses a where clause when given" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      newFooBars = [mkFooBar 2 "cat"]

      deleteDogs =
        Expr.deleteExpr
          fooBarTable
          (Just (Expr.whereClause (Expr.equals barColumnRef (Expr.valueExpression (SqlValue.fromText (T.pack "dog"))))))
          Nothing

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          RawSql.executeVoid connection deleteDogs

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows newFooBars

prop_deleteExprWithReturning :: Property.NamedDBProperty
prop_deleteExprWithReturning =
  Property.singletonNamedDBProperty "deleteExpr with returning returns the original rows" $ \pool -> do
    let
      oldFooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

      deleteDogs =
        Expr.deleteExpr
          fooBarTable
          Nothing
          (Just $ Expr.returningExpr $ Expr.selectColumns [fooColumn, barColumn])

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing Nothing

          result <- RawSql.execute connection deleteDogs

          Execution.readRows result

    assertEqualFooBarRows rows oldFooBars

prop_truncateTablesExpr :: Property.NamedDBProperty
prop_truncateTablesExpr =
  Property.singletonNamedDBProperty "truncateTablesExpr clears out inserted values from all tables" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]
      fooBar2Table =
        Expr.unqualified (Expr.tableName "foobar2")
      dropAndRecreateSecondTable connection = do
        RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql fooBar2Table)
        RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql fooBar2Table <> RawSql.fromString "(foo INTEGER, bar TEXT)")

      recreateAndInsert =
        MIO.liftIO $
          Conn.withPoolConnection pool $ \connection -> do
            dropAndRecreateTestTable connection
            dropAndRecreateSecondTable connection

            RawSql.executeVoid connection $
              Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing
            RawSql.executeVoid connection $
              Expr.insertExpr fooBar2Table Nothing (insertFooBarSource fooBars) Nothing Nothing

      truncateTables =
        MIO.liftIO $
          Conn.withPoolConnection pool $ \connection -> do
            RawSql.executeVoid connection $
              Expr.truncateTablesExpr (fooBarTable :| (pure fooBar2Table))

    _ <- recreateAndInsert
    _ <- truncateTables

    fooBarRows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    fooBar2Rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          result <- RawSql.execute connection $ findAllFooBarsInTable fooBar2Table

          Execution.readRows result

    assertEqualFooBarRows fooBarRows []
    assertEqualFooBarRows fooBar2Rows []
