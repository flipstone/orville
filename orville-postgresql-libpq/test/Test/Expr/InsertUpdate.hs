module Test.Expr.InsertUpdate
  ( insertUpdateTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualSqlRows, barColumn, dropAndRecreateTestTable, encodeFooBar, fooBarTable, fooColumn, insertFooBarSource, orderByFoo)
import qualified Test.Property as Property

insertUpdateTests :: Pool.Pool Conn.Connection -> IO Bool
insertUpdateTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "Expr - Insert/Update")
      [
        ( String.fromString "insertExpr inserts values"
        , Property.singletonProperty $ do
            let fooBars = [FooBar 1 "dog", FooBar 2 "cat"]

            rows <-
              MIO.liftIO $
                Pool.withResource pool $ \connection -> do
                  dropAndRecreateTestTable connection

                  RawSql.executeVoid connection $
                    Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars)

                  result <-
                    RawSql.execute connection $
                      Expr.queryExpr
                        (Expr.selectClause $ Expr.selectExpr Nothing)
                        (Expr.selectColumns [fooColumn, barColumn])
                        (Just $ Expr.tableExpr fooBarTable Nothing Nothing Nothing Nothing Nothing)

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

            rows <-
              MIO.liftIO $
                Pool.withResource pool $ \connection -> do
                  dropAndRecreateTestTable connection

                  RawSql.executeVoid connection $
                    Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars)

                  RawSql.executeVoid connection $
                    setBarToFerret

                  result <-
                    RawSql.execute connection $
                      Expr.queryExpr
                        (Expr.selectClause $ Expr.selectExpr Nothing)
                        (Expr.selectColumns [fooColumn, barColumn])
                        (Just $ Expr.tableExpr fooBarTable Nothing (Just orderByFoo) Nothing Nothing Nothing)

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

            rows <-
              MIO.liftIO $
                Pool.withResource pool $ \connection -> do
                  dropAndRecreateTestTable connection

                  RawSql.executeVoid connection $
                    Expr.insertExpr fooBarTable Nothing (insertFooBarSource oldFooBars)

                  RawSql.executeVoid connection $
                    updateDogToForret

                  result <-
                    RawSql.execute connection $
                      Expr.queryExpr
                        (Expr.selectClause $ Expr.selectExpr Nothing)
                        (Expr.selectColumns [fooColumn, barColumn])
                        (Just $ Expr.tableExpr fooBarTable Nothing (Just orderByFoo) Nothing Nothing Nothing)

                  ExecResult.readRows result

            rows `assertEqualSqlRows` map encodeFooBar newFooBars
        )
      ]
