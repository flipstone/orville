{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | Tables, plans, and data generation shared by the jsonplan test modules.
module Test.Fixtures
  ( Author (..)
  , Book (..)
  , authorTable
  , authorIdField
  , authorNameField
  , authorShelfField
  , bookTable
  , bookIdField
  , bookAuthorIdField
  , bookTitleField
  , bookInPrintField
  , authorWithBooks
  , maybeAuthor
  , authorLibrary
  , flatReport
  , FlatReportRow (..)
  , booksByAuthorId
  , Dataset (..)
  , genAdversarialText
  , genDataset
  , genHitParams
  , genMixedParams
  , seedDataset
  , createTestPool
  , createSchema
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.Environment as Env

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Orville.JsonPlan as JP

data Author = Author
  { authorId :: Int.Int32
  , authorName :: T.Text
  , authorShelf :: T.Text
  }
  deriving (Eq, Show)

data Book = Book
  { bookId :: Int.Int32
  , bookAuthorId :: Int.Int32
  , bookTitle :: T.Text
  , bookInPrint :: Bool
  }
  deriving (Eq, Show)

authorIdField :: O.FieldDefinition O.NotNull Int.Int32
authorIdField =
  O.integerField "id"

authorNameField :: O.FieldDefinition O.NotNull T.Text
authorNameField =
  O.unboundedTextField "name"

authorShelfField :: O.FieldDefinition O.NotNull T.Text
authorShelfField =
  O.fixedTextField "shelf" 3

authorTable :: O.TableDefinition (O.HasKey Int.Int32) Author Author
authorTable =
  O.mkTableDefinition
    "jpt_author"
    (O.primaryKey authorIdField)
    ( Author
        <$> O.marshallField authorId authorIdField
        <*> O.marshallField authorName authorNameField
        <*> O.marshallField authorShelf authorShelfField
    )

bookIdField :: O.FieldDefinition O.NotNull Int.Int32
bookIdField =
  O.integerField "id"

bookAuthorIdField :: O.FieldDefinition O.NotNull Int.Int32
bookAuthorIdField =
  O.integerField "author_id"

bookTitleField :: O.FieldDefinition O.NotNull T.Text
bookTitleField =
  O.unboundedTextField "title"

bookInPrintField :: O.FieldDefinition O.NotNull Bool
bookInPrintField =
  O.booleanField "in_print"

bookTable :: O.TableDefinition (O.HasKey Int.Int32) Book Book
bookTable =
  O.mkTableDefinition
    "jpt_book"
    (O.primaryKey bookIdField)
    ( Book
        <$> O.marshallField bookId bookIdField
        <*> O.marshallField bookAuthorId bookAuthorIdField
        <*> O.marshallField bookTitle bookTitleField
        <*> O.marshallField bookInPrint bookInPrintField
    )

authorWithBooks :: JP.JsonPlan ref T.Text (Author, [Book])
authorWithBooks = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  books <- JP.findAll bookTable bookAuthorIdField (JP.refField authorId authorIdField author)
  JP.pair author books

maybeAuthor :: JP.JsonPlan ref T.Text (Maybe Author)
maybeAuthor =
  JP.findMaybeOne authorTable authorNameField JP.rootParam

authorLibrary :: JP.JsonPlan ref T.Text [(Book, Author)]
authorLibrary = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  JP.findAllEach bookTable bookAuthorIdField (JP.refField authorId authorIdField author) $
    \book -> JP.do
      bookAuthor <-
        JP.findOne authorTable authorIdField (JP.refField bookAuthorId bookAuthorIdField book)
      JP.result ((,) <$> JP.refValue book <*> JP.refValue bookAuthor)

data FlatReportRow = FlatReportRow
  { flatReportRowName :: T.Text
  , flatReportRowBookCount :: Int.Int32
  , flatReportRowTitles :: T.Text
  }
  deriving (Eq, Show)

flatReport :: JP.JsonPlan ref T.Text FlatReportRow
flatReport = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  JP.flat
    ( FlatReportRow
        <$> JP.refCol authorName authorNameField author
        <*> JP.aggCol bookTable bookAuthorIdField (JP.refField authorId authorIdField author) countBooks
        <*> JP.aggCol bookTable bookAuthorIdField (JP.refField authorId authorIdField author) joinedTitles
    )

countBooks :: JP.Aggregation Book Int.Int32
countBooks =
  JP.aggregation
    (fromIntegral . length)
    (RawSql.fromString "count(*)::int")
    O.integer

joinedTitles :: JP.Aggregation Book T.Text
joinedTitles =
  JP.aggregation
    (T.intercalate (T.pack ", ") . fmap bookTitle . List.sortOn bookId)
    (RawSql.fromString "coalesce(string_agg(t.\"title\", ', ' ORDER BY t.\"id\"), '')")
    O.unboundedText

booksByAuthorId :: JP.JsonPlan ref Int.Int32 [Book]
booksByAuthorId =
  JP.findAll bookTable bookAuthorIdField JP.rootParam

data Dataset = Dataset
  { datasetAuthors :: [Author]
  , datasetBooks :: [Book]
  }
  deriving Show

genAdversarialText :: Int -> HH.Gen T.Text
genAdversarialText maxLength =
  let
    genChar =
      Gen.frequency
        [ (4, Gen.filter (/= '\NUL') Gen.unicode)
        , (2, Gen.element ['"', '\\', ',', ' ', '\'', '{', '}', '(', ')', ':', '[', ']'])
        ]
  in
    Gen.text (Range.linear 0 maxLength) genChar

genDataset :: HH.Gen Dataset
genDataset = do
  authorCount <- Gen.int (Range.linear 1 4)
  authors <-
    traverse
      ( \index -> do
          nameBase <- genAdversarialText 12
          shelf <- genAdversarialText 3
          pure $
            Author
              (fromIntegral index)
              (nameBase <> T.pack ("#" <> show index))
              shelf
      )
      [1 .. authorCount]
  bookCount <- Gen.int (Range.linear 0 8)
  books <-
    traverse
      ( \index -> do
          ownerId <-
            Gen.frequency
              [ (5, Gen.element (fmap authorId authors))
              , (1, pure 999)
              ]
          title <- genAdversarialText 20
          inPrint <- Gen.bool
          pure (Book (fromIntegral (100 + index)) ownerId title inPrint)
      )
      [1 .. bookCount]
  pure (Dataset authors books)

-- | Parameters drawn from existing author names, with repeats and in any order.
genHitParams :: Dataset -> HH.Gen [T.Text]
genHitParams dataset =
  Gen.list
    (Range.linear 1 6)
    (Gen.element (fmap authorName (datasetAuthors dataset)))

-- | Parameters mixing existing author names with names that match nothing.
genMixedParams :: Dataset -> HH.Gen [T.Text]
genMixedParams dataset =
  let
    hits = fmap authorName (datasetAuthors dataset)
    misses = [T.pack "missing#1", T.pack "missing#2"]
  in
    Gen.list (Range.linear 1 6) (Gen.element (hits <> misses))

seedDataset :: O.MonadOrville m => Dataset -> m ()
seedDataset dataset = do
  O.executeVoid Exec.DeleteQuery (RawSql.fromString "DELETE FROM jpt_book")
  O.executeVoid Exec.DeleteQuery (RawSql.fromString "DELETE FROM jpt_author")
  Foldable.traverse_
    (O.insertEntities O.InOneStatement authorTable)
    (NEL.nonEmpty (datasetAuthors dataset))
  Foldable.traverse_
    (O.insertEntities O.InOneStatement bookTable)
    (NEL.nonEmpty (datasetBooks dataset))

createTestPool :: IO O.ConnectionPool
createTestPool = do
  mbConnString <- Env.lookupEnv "JSONPLAN_CONN"
  let
    connString =
      Maybe.fromMaybe
        "host=localhost port=15432 user=orville_test password=orville"
        mbConnString
  O.createConnectionPool
    O.ConnectionOptions
      { O.connectionString = connString
      , O.connectionNoticeReporting = O.DisableNoticeReporting
      , O.connectionPoolStripes = O.OneStripePerCapability
      , O.connectionPoolLingerTime = 10
      , O.connectionPoolMaxConnections = O.MaxConnectionsTotal 4
      }

createSchema :: O.MonadOrville m => m ()
createSchema = do
  O.executeVoid Exec.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS jpt_book")
  O.executeVoid Exec.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS jpt_author")
  O.executeVoid Exec.DDLQuery $
    RawSql.fromString
      "CREATE TABLE jpt_author (id integer PRIMARY KEY, name text NOT NULL, shelf char(3) NOT NULL)"
  O.executeVoid Exec.DDLQuery $
    RawSql.fromString
      "CREATE TABLE jpt_book (id integer PRIMARY KEY, author_id integer NOT NULL, title text NOT NULL, in_print boolean NOT NULL)"
