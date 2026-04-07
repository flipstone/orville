module Test.TableDefinition
  ( tableDefinitionTests
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Batchable as Batchable
import qualified Orville.PostgreSQL.Execution.ReturningOption as ReturningOption
import qualified Orville.PostgreSQL.Execution.Select as Select
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Schema as Schema
import qualified Orville.PostgreSQL.Schema.ConstraintDefinition as ConstraintDefinition
import qualified Orville.PostgreSQL.Schema.TableDefinition as TableDefinition

import qualified Test.Entities.Bar as Bar
import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

tableDefinitionTests :: Orville.ConnectionPool -> Property.Group
tableDefinitionTests pool =
  Property.group
    "TableDefinition"
    [ prop_roundTrip pool
    , prop_readOnlyFields pool
    , prop_primaryKey pool
    , prop_checkConstraint pool
    , prop_uniqueConstraint pool
    , prop_fieldConstraints
    , prop_insertDefaultValuesWithEmptyMarshaller pool
    , prop_autoSizedBatchedInsert pool
    , prop_autoSizedBatchedInsertCountsOnConflictParams
    ]

prop_roundTrip :: Property.NamedDBProperty
prop_roundTrip =
  Property.namedDBProperty "Creates a table that can round trip an entity through it" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let
      insertFoo =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            (originalFoo NE.:| [])

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

    let
      insertBar =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Bar.table
            Nothing
            (originalBar :| [])

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

    let
      conflictingFoo =
        originalFoo {Foo.fooName = T.reverse $ Foo.fooName originalFoo}

      insertFoos =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            (originalFoo :| [conflictingFoo])

    result <- MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection -> do
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

    let
      fooTableWithUniqueNameConstraint =
        Orville.addTableConstraints
          [Orville.uniqueConstraint (Orville.fieldName Foo.fooNameField :| [])]
          Foo.table

      conflictingFoo =
        originalFoo {Foo.fooId = 1 + Foo.fooId originalFoo}

      insertFoos =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            (originalFoo :| [conflictingFoo])

    result <- MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection fooTableWithUniqueNameConstraint
      RawSql.executeVoid connection insertFoos

    case result of
      Right () -> do
        HH.footnote "Expected 'executeVoid' to return failure, but it did not"
        HH.failure
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "23505")

prop_checkConstraint :: Property.NamedDBProperty
prop_checkConstraint =
  Property.singletonNamedDBProperty "Creates a check constraint and checks a valid and invalid insert" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let
      fooTableWithUniqueNameConstraint =
        Orville.addTableConstraints
          [Orville.checkConstraint (Schema.unqualifiedNameToConstraintId "fooPositiveAgeConstraint") $ RawSql.fromString "age > 5"]
          Foo.table

      insertFoo foo =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            (pure foo)

      validFoo =
        originalFoo {Foo.fooAge = 6}

    MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection fooTableWithUniqueNameConstraint
      RawSql.executeVoid connection $ insertFoo validFoo

    let
      invalidFoo =
        originalFoo {Foo.fooAge = 4}

    invalidResult <- MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection fooTableWithUniqueNameConstraint
      RawSql.executeVoid connection $ insertFoo invalidFoo

    case invalidResult of
      Right () -> do
        HH.footnote "Expected 'executeVoid' to return failure, but it did not"
        HH.failure
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "23514")

prop_fieldConstraints :: Property.NamedProperty
prop_fieldConstraints =
  Property.singletonNamedProperty "Includes field constraints in table constraints" $ do
    let
      foreignTableId =
        Orville.unqualifiedNameToTableId "foreign_table"

      foreignFieldName =
        Orville.stringToFieldName "foreign_field"

      fieldWithoutConstraints =
        Orville.integerField "foo"

      fieldWithConstraints =
        Orville.addForeignKeyConstraint foreignTableId foreignFieldName
          . Orville.addUniqueConstraint
          $ fieldWithoutConstraints

      tableWithFieldConstraints =
        Orville.mkTableDefinitionWithoutKey
          "test_table"
          (Orville.marshallField id fieldWithConstraints)

      fieldName =
        Orville.fieldName fieldWithoutConstraints

      tableWithTableConstraints =
        Orville.addTableConstraints
          [ Orville.foreignKeyConstraint
              foreignTableId
              (Orville.foreignReference fieldName foreignFieldName :| [])
          , Orville.uniqueConstraint (fieldName :| [])
          ]
          $ Orville.mkTableDefinitionWithoutKey
            "test_table"
            (Orville.marshallField id fieldWithoutConstraints)

      tableConstraintKeys ::
        Orville.TableDefinition hasKey writeEntity readEntity ->
        Set.Set Orville.ConstraintMigrationKey
      tableConstraintKeys =
        ConstraintDefinition.tableConstraintKeys . Orville.tableConstraints

    tableConstraintKeys tableWithFieldConstraints === tableConstraintKeys tableWithTableConstraints

prop_insertDefaultValuesWithEmptyMarshaller :: Property.NamedDBProperty
prop_insertDefaultValuesWithEmptyMarshaller =
  Property.singletonNamedDBProperty "Inserts default values when table marshaller is empty" $ \pool -> do
    let
      table :: TableDefinition.TableDefinition (Orville.HasKey Bar.BarId) () ()
      table =
        TableDefinition.mapTableMarshaller (const $ pure ()) Bar.table
      entities =
        NE.fromList [(), (), ()]
      expectedResult =
        [ Bar.Bar 1 $ T.pack "default"
        , Bar.Bar 2 $ T.pack "default"
        , Bar.Bar 3 $ T.pack "default"
        ]

      insertExpr =
        Batchable.toUnbatched $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            table
            Nothing
            entities

    result <- MIO.liftIO . Orville.runOrville pool $ do
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        TestTable.dropAndRecreateTableDef connection Bar.table
        RawSql.executeVoid connection insertExpr
      Select.executeSelect $ Select.selectTable Bar.table mempty

    result === expectedResult

prop_autoSizedBatchedInsert :: Property.NamedDBProperty
prop_autoSizedBatchedInsert =
  Property.singletonNamedDBProperty "Auto-sized batched insert keeps parameter count below postgresql max" $ \pool -> do
    let
      columnCount =
        Orville.foldMarshallerFields
          Foo.fooMarshaller
          0
          (const (+ 1))

      entityCount =
        -- postgresql allows 65535 params and we will include one param per column
        70000 `div` columnCount

      mkFoo n =
        Foo.Foo n (T.pack ("Foo " <> show n)) n

      entities =
        mkFoo <$> (1 :| [2 .. entityCount])

      insertExprs =
        Batchable.toBatched Batchable.BatchSizeAuto $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            entities

    MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection Foo.table
      traverse_ (RawSql.executeVoid connection) insertExprs

prop_autoSizedBatchedInsertCountsOnConflictParams :: Property.NamedProperty
prop_autoSizedBatchedInsertCountsOnConflictParams =
  Property.singletonNamedProperty "Auto-sized batched insert accounts for on conflict params" $ do
    let
      columnCount =
        Orville.foldMarshallerFields
          Foo.fooMarshaller
          0
          (const (+ 1))

      entityCount =
        -- Use enough entities that the on conflict params will push us past a
        -- batch boundary. Without on conflict: 65535/3 = 21845 per batch.
        -- With 10000 on conflict params: (65535-10000)/3 = 18511 per batch.
        -- 40000 entities = 2 batches without, 3 batches with.
        120000 `div` columnCount

      mkFoo n =
        Foo.Foo n (T.pack ("Foo " <> show n)) n

      entities =
        mkFoo <$> (1 :| [2 .. entityCount])

      -- Build an OnConflictExpr that contains parameters. The exact SQL
      -- doesn't need to be valid for this table — we just need it to have
      -- a known parameter count so we can verify the batching accounts for it.
      onConflictParamCount =
        10000

      onConflictWithParams =
        mconcat
          . replicate onConflictParamCount
          $ RawSql.parameter (SqlValue.fromInt32 0)

      onConflictExpr :: Expr.OnConflictExpr
      onConflictExpr =
        RawSql.unsafeFromRawSql
          ( RawSql.fromString "ON CONFLICT DO NOTHING "
              <> onConflictWithParams
          )

      batchesWithoutOnConflict =
        Batchable.toBatched Batchable.BatchSizeAuto $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            Nothing
            entities

      batchesWithOnConflict =
        Batchable.toBatched Batchable.BatchSizeAuto $
          TableDefinition.mkInsertExpr
            ReturningOption.WithoutReturning
            Foo.table
            (Just onConflictExpr)
            entities

    HH.footnote $ "Batches without OnConflict: " <> show (length batchesWithoutOnConflict)
    HH.footnote $ "Batches with OnConflict: " <> show (length batchesWithOnConflict)

    -- With the OnConflict params consuming available parameter slots, there
    -- should be more batches needed.
    HH.assert (length batchesWithOnConflict > length batchesWithoutOnConflict)
