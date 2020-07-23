module QuotedFields.QuoteTest
( test_quotedFields
) where

import qualified Database.Orville.PostgreSQL as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import QuotedFields.Data.Entity (NonNullableEntity(..))
import QuotedFields.Schema.Entity (nonNullableEntityTable, nullableEntityTable, droppedEntityTable)
import qualified TestDB as TestDB

test_quotedFields :: TestTree
test_quotedFields =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "quoted field tests"
      [ testCase "Create table" $ do
          run (TestDB.reset [])
          run (O.migrateSchema [O.Table nonNullableEntityTable])

      , testCase "Alter columns" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          run (O.migrateSchema [O.Table nullableEntityTable])
          run (O.migrateSchema [O.Table nonNullableEntityTable])

      , testCase "Drop and add columns" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          run (O.migrateSchema [O.Table droppedEntityTable])
          run (O.migrateSchema [O.Table nonNullableEntityTable])

      , testCase "Drop table" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          run (O.migrateSchema [O.Table nonNullableEntityTable, O.DropTable "entity"])

      , testCase "Insert and find" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          insertedEntity <- run $ O.insertRecord nonNullableEntityTable (NonNullableEntity 1 2 3)
          foundEntity <- run $ O.findRecord nonNullableEntityTable (nonNullSnakeCase insertedEntity)
          assertEqual
            "Entity found in database didn't match the originally inserted values"
            (Just insertedEntity)
            foundEntity

      , testCase "Update" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          insertedEntity <- run $ O.insertRecord nonNullableEntityTable (NonNullableEntity 1 2 3)
          let insertedKey = nonNullSnakeCase insertedEntity
          let updatedEntity = NonNullableEntity insertedKey 3 4
          run $ O.updateRecord nonNullableEntityTable insertedKey updatedEntity
          newlyFoundEntity <-
            run $ O.findRecord nonNullableEntityTable insertedKey
          assertEqual
            "Entity found in database didn't match the values passed in for update"
            (Just updatedEntity)
            newlyFoundEntity

      , testCase "Delete" $ do
          run (TestDB.reset [O.Table nonNullableEntityTable])
          insertedEntity <- run $ O.insertRecord nonNullableEntityTable (NonNullableEntity 1 2 3)
          let insertedKey = nonNullSnakeCase insertedEntity
          run $ O.deleteRecord nonNullableEntityTable insertedKey
          newlyFoundEntity <- run $ O.findRecord nonNullableEntityTable insertedKey
          assertEqual
            "Entity found in database after deleted"
            Nothing
            newlyFoundEntity
      ]
