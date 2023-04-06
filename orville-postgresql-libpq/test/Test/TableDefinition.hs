module Test.TableDefinition
  ( tableDefinitionTests,
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Execution.ReturningOption as ReturningOption
import qualified Orville.PostgreSQL.Execution.Select as Select
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema.TableDefinition as TableDefinition

import qualified Test.Entities.Bar as Bar
import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

tableDefinitionTests :: Pool.Pool Conn.Connection -> Property.Group
tableDefinitionTests pool =
  Property.group
    "TableDefinition"
    [ prop_roundTrip pool
    , prop_readOnlyFields pool
    , prop_primaryKey pool
    , prop_uniqueConstraint pool
    ]

prop_roundTrip :: Property.NamedDBProperty
prop_roundTrip =
  Property.namedDBProperty "Creates a table that can round trip an entity through it" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let insertFoo =
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            (originalFoo :| [])

        selectFoos =
          Select.selectTable Foo.table mempty

    foosFromDB <-
      MIO.liftIO . Orville.runOrville pool $ do
        Orville.withConnection $ \connection -> do
          MIO.liftIO $ TestTable.dropAndRecreateTableDef connection Foo.table
        Orville.executeVoid Orville.InsertQuery insertFoo
        Select.executeSelect selectFoos

    foosFromDB === [originalFoo]

prop_readOnlyFields :: Property.NamedDBProperty
prop_readOnlyFields =
  Property.namedDBProperty "Creates a table that can read from read only fields" $ \pool -> do
    originalBar <- HH.forAll Bar.generate

    let insertBar =
          TableDefinition.mkInsertExpr ReturningOption.WithoutReturning Bar.table (originalBar :| [])

        selectBars =
          Select.selectTable Bar.table mempty

    barsFromDB <-
      MIO.liftIO . Orville.runOrville pool $ do
        Orville.withConnection $ \connection -> do
          MIO.liftIO $ TestTable.dropAndRecreateTableDef connection Bar.table
        Orville.executeVoid Orville.InsertQuery insertBar
        Select.executeSelect selectBars

    fmap Bar.barName barsFromDB === [Bar.barName originalBar]

prop_primaryKey :: Property.NamedDBProperty
prop_primaryKey =
  Property.singletonNamedDBProperty "Creates a primary key that rejects duplicate records" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let conflictingFoo =
          originalFoo {Foo.fooName = T.reverse $ Foo.fooName originalFoo}

        insertFoos =
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            (originalFoo :| [conflictingFoo])

    result <- MIO.liftIO . E.try . Pool.withResource pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection Foo.table
      RawSql.executeVoid connection insertFoos

    case result of
      Right () -> do
        HH.footnote "Expected 'executeVoid' to return failure, but it did not"
        HH.failure
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "23505")

prop_uniqueConstraint :: Property.NamedDBProperty
prop_uniqueConstraint =
  Property.singletonNamedDBProperty "Creates a unique constraint that rejects duplicate records" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let fooTableWithUniqueNameConstraint =
          Orville.addTableConstraints
            [Orville.uniqueConstraint (Orville.fieldName Foo.fooNameField :| [])]
            Foo.table

        conflictingFoo =
          originalFoo {Foo.fooId = 1 + Foo.fooId originalFoo}

        insertFoos =
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            (originalFoo :| [conflictingFoo])

    result <- MIO.liftIO . E.try . Pool.withResource pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection fooTableWithUniqueNameConstraint
      RawSql.executeVoid connection insertFoos

    case result of
      Right () -> do
        HH.footnote "Expected 'executeVoid' to return failure, but it did not"
        HH.failure
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "23505")
