module Test.Expr.InsertUpdateDelete
  ( insertUpdateDeleteTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, findAllFooBars, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property
import qualified Test.ReadRows as ReadRows

insertUpdateDeleteTests :: Orville.Pool Orville.Connection -> Property.Group
insertUpdateDeleteTests pool =
  Property.group
    "Expr - Insert/Update/Delete"
    [ prop_insertExpr pool
    , prop_insertExprWithReturning pool
    , prop_updateExpr pool
    , prop_updateExprWithWhere pool
    , prop_updateExprWithReturning pool
    , prop_deleteExpr pool
    , prop_deleteExprWithWhere pool
    , prop_deleteExprWithReturning pool
    ]

prop_insertExpr :: Property.NamedDBProperty
prop_insertExpr =
  Property.singletonNamedDBProperty "insertExpr inserts values" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing

          result <- RawSql.execute connection findAllFooBars

          ReadRows.readRows result

    assertEqualFooBarRows rows fooBars

prop_insertExprWithReturning :: Property.NamedDBProperty
prop_insertExprWithReturning =
  Property.singletonNamedDBProperty "insertExpr with returning clause returns the requested columns" $ \pool -> do
    let
      fooBars = [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          result <-
            RawSql.execute connection $
              Expr.insertExpr
                fooBarTable
                Nothing
                (insertFooBarSource fooBars)
                (Just $ Expr.returningExpr $ Expr.selectColumns [fooColumn, barColumn])

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          RawSql.executeVoid connection setBarToFerret

          result <- RawSql.execute connection findAllFooBars

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          RawSql.executeVoid connection updateDogToForret

          result <- RawSql.execute connection findAllFooBars

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          result <- RawSql.execute connection setBarToFerret

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          RawSql.executeVoid connection deleteRows

          result <- RawSql.execute connection findAllFooBars

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          RawSql.executeVoid connection deleteDogs

          result <- RawSql.execute connection findAllFooBars

          ReadRows.readRows result

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
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

          result <- RawSql.execute connection deleteDogs

          ReadRows.readRows result

    assertEqualFooBarRows rows oldFooBars
