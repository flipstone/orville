module Test.Expr.InsertUpdateDelete
  ( insertUpdateDeleteTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualSqlRows, barColumn, dropAndRecreateTestTable, encodeFooBar, findAllFooBars, fooBarTable, fooColumn, insertFooBarSource)
import qualified Test.Property as Property

insertUpdateDeleteTests :: Pool.Pool Conn.Connection -> Property.Group
insertUpdateDeleteTests pool =
  Property.group
    "Expr - Insert/Update/Delete"
    [
      ( String.fromString "insertExpr inserts values"
      , Property.singletonProperty $ do
          let fooBars = [FooBar 1 "dog", FooBar 2 "cat"]

          rows <-
            MIO.liftIO $
              Pool.withResource pool $ \connection -> do
                dropAndRecreateTestTable connection

                RawSql.executeVoid connection $
                  Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing

                result <- RawSql.execute connection findAllFooBars

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar fooBars
      )
    ,
      ( String.fromString "insertExpr with returning clause returns the requested columns"
      , Property.singletonProperty $ do
          let fooBars = [FooBar 1 "dog", FooBar 2 "cat"]

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
                      (Just $ Expr.returningExpr [fooColumn, barColumn])

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar fooBars
      )
    ,
      ( String.fromString "updateExpr updates rows in the db"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]
              newFooBars = [FooBar 1 "ferret", FooBar 2 "ferret"]

              setBarToFerret =
                Expr.updateExpr
                  fooBarTable
                  (Expr.setClauseList [Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret"))])
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

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar newFooBars
      )
    ,
      ( String.fromString "updateExpr uses a where clause when given"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]
              newFooBars = [FooBar 1 "ferret", FooBar 2 "cat"]

              updateDogToForret =
                Expr.updateExpr
                  fooBarTable
                  (Expr.setClauseList [Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret"))])
                  (Just (Expr.whereClause (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))))
                  Nothing

          rows <-
            MIO.liftIO $
              Pool.withResource pool $ \connection -> do
                dropAndRecreateTestTable connection

                RawSql.executeVoid connection $
                  Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

                RawSql.executeVoid connection updateDogToForret

                result <- RawSql.execute connection findAllFooBars

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar newFooBars
      )
    ,
      ( String.fromString "updateExpr with returning clause returns the new records"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]
              newFooBars = [FooBar 1 "ferret", FooBar 2 "ferret"]

              setBarToFerret =
                Expr.updateExpr
                  fooBarTable
                  (Expr.setClauseList [Expr.setColumn barColumn (SqlValue.fromText (T.pack "ferret"))])
                  Nothing
                  (Just $ Expr.returningExpr [fooColumn, barColumn])

          rows <-
            MIO.liftIO $
              Pool.withResource pool $ \connection -> do
                dropAndRecreateTestTable connection

                RawSql.executeVoid connection $
                  Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

                result <- RawSql.execute connection setBarToFerret

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar newFooBars
      )
    ,
      ( String.fromString "deleteExpr updates rows in the db"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]

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

                ExecResult.readRows result

          rows `assertEqualSqlRows` []
      )
    ,
      ( String.fromString "deleteExpr uses a where clause when given"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]
              newFooBars = [FooBar 2 "cat"]

              deleteDogs =
                Expr.deleteExpr
                  fooBarTable
                  (Just (Expr.whereClause (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))))
                  Nothing

          rows <-
            MIO.liftIO $
              Pool.withResource pool $ \connection -> do
                dropAndRecreateTestTable connection

                RawSql.executeVoid connection $
                  Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

                RawSql.executeVoid connection deleteDogs

                result <- RawSql.execute connection findAllFooBars

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar newFooBars
      )
    ,
      ( String.fromString "deleteExpr with returning returns the original rows"
      , Property.singletonProperty $ do
          let oldFooBars = [FooBar 1 "dog", FooBar 2 "cat"]

              deleteDogs =
                Expr.deleteExpr
                  fooBarTable
                  Nothing
                  (Just $ Expr.returningExpr [fooColumn, barColumn])

          rows <-
            MIO.liftIO $
              Pool.withResource pool $ \connection -> do
                dropAndRecreateTestTable connection

                RawSql.executeVoid connection $
                  Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars) Nothing

                result <- RawSql.execute connection deleteDogs

                ExecResult.readRows result

          rows `assertEqualSqlRows` map encodeFooBar oldFooBars
      )
    ]
