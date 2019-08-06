module ErrorsTest where

import Control.Exception.Lifted (try)
import Control.Monad (void)
import Data.Int (Int32)

import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Select as S

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import ParameterizedEntity.Data.Virus (VirusId, bpsVirus)

import ParameterizedEntity.Schema
  ( schema
  , virusIdField
  , virusNameField
  , virusTable
  )
import qualified TestDB as TestDB

test_errors :: TestTree
test_errors =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Errors Test"
      [ testCase "RowDataError is raised on invalid row" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord virusTable bpsVirus)
          let badSelect =
                S.selectQuery
                  badVirusFromSql
                  (S.fromClauseTable virusTable)
                  mempty
          result <- run (try (S.runSelect badSelect))
          case result of
            Left (O.RowDataError _) -> pure ()
            Left err ->
              assertFailure
                ("Expected RowDataError, but got this error: " ++ show err)
            Right rows ->
              assertFailure
                ("Expected RowDataError, but got these rows: " ++ show rows)
      ]

data BadVirus = BadVirus
  { badVirusId :: VirusId
  , badVirusName :: Int32 -- Virus name is actually Text, not Int32!
  } deriving (Show)

badVirusNameField :: O.FieldDefinition Int32
badVirusNameField = virusNameField `O.withConversion` const O.integer

badVirusFromSql :: O.FromSql BadVirus
badVirusFromSql =
  BadVirus <$> O.fieldFromSql virusIdField <*> O.fieldFromSql badVirusNameField
