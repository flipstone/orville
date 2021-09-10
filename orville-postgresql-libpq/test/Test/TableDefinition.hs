module Test.TableDefinition
  ( tableDefinitionTests,
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NEL
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.Select as Select
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition

import qualified Test.Entities.Bar as Bar
import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

tableDefinitionTests :: Pool.Pool Conn.Connection -> Property.Group
tableDefinitionTests pool =
  Property.group "TableDefinition" $
    [
      ( String.fromString "Creates a table that can round trip an entity through it"
      , HH.property $ do
          originalFoo <- HH.forAll Foo.generate

          let insertFoo =
                TableDefinition.mkInsertExpr
                  TableDefinition.WithoutReturning
                  Foo.table
                  (originalFoo NEL.:| [])

              selectFoos =
                Select.selectTable Foo.table mempty

          foosFromDB <-
            MIO.liftIO . Orville.runOrville pool $ do
              Orville.withConnection $ \connection -> do
                MIO.liftIO $ TestTable.dropAndRecreateTableDef connection Foo.table
              Orville.executeVoid insertFoo
              Select.executeSelect selectFoos

          foosFromDB HH.=== [originalFoo]
      )
    ,
      ( String.fromString "Creates a table that can read from read only fields"
      , HH.property $ do
          originalBar <- HH.forAll Bar.generate

          let insertBar =
                TableDefinition.mkInsertExpr TableDefinition.WithoutReturning Bar.table (originalBar NEL.:| [])

              selectBars =
                Select.selectTable Bar.table mempty

          barsFromDB <-
            MIO.liftIO . Orville.runOrville pool $ do
              Orville.withConnection $ \connection -> do
                MIO.liftIO $ TestTable.dropAndRecreateTableDef connection Bar.table
              Orville.executeVoid insertBar
              Select.executeSelect selectBars

          (fmap Bar.barName) barsFromDB HH.=== [Bar.barName originalBar]
      )
    ,
      ( String.fromString "Creates a primary key that rejects duplicate records"
      , Property.singletonProperty $ do
          originalFoo <- HH.forAll Foo.generate

          let insertFoo =
                TableDefinition.mkInsertExpr
                  TableDefinition.WithoutReturning
                  Foo.table
                  (originalFoo NEL.:| [])

          result <- MIO.liftIO . E.try . Pool.withResource pool $ \connection -> do
            TestTable.dropAndRecreateTableDef connection Foo.table
            RawSql.executeVoid connection insertFoo
            RawSql.executeVoid connection insertFoo

          case result of
            Right () -> do
              HH.footnote "Expected 'executeVoid' to return failure, but it did not"
              HH.failure
            Left err ->
              Conn.sqlExecutionErrorSqlState err HH.=== Just (B8.pack "23505")
      )
    ]
