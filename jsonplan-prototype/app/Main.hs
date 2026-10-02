{-# LANGUAGE QualifiedDo #-}

module Main
  ( main
  ) where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Int as Int
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe as Maybe
import qualified Data.Profunctor as Profunctor
import qualified Data.Text as T
import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Orville.JsonPlan as JP

data Author = Author
  { authorId :: Int.Int32
  , authorName :: T.Text
  }
  deriving (Eq, Show)

data Book = Book
  { bookId :: Int.Int32
  , bookAuthorId :: Int.Int32
  , bookTitle :: T.Text
  }
  deriving (Eq, Show)

authorIdField :: O.FieldDefinition O.NotNull Int.Int32
authorIdField =
  O.integerField "id"

authorNameField :: O.FieldDefinition O.NotNull T.Text
authorNameField =
  O.unboundedTextField "name"

authorTable :: O.TableDefinition (O.HasKey Int.Int32) Author Author
authorTable =
  O.mkTableDefinition
    "jsonplan_author"
    (O.primaryKey authorIdField)
    ( Author
        <$> O.marshallField authorId authorIdField
        <*> O.marshallField authorName authorNameField
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

bookTable :: O.TableDefinition (O.HasKey Int.Int32) Book Book
bookTable =
  O.mkTableDefinition
    "jsonplan_book"
    (O.primaryKey bookIdField)
    ( Book
        <$> O.marshallField bookId bookIdField
        <*> O.marshallField bookAuthorId bookAuthorIdField
        <*> O.marshallField bookTitle bookTitleField
    )

authorWithBooks :: JP.JsonPlan ref T.Text (Author, [Book])
authorWithBooks = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  books <- JP.findAll bookTable bookAuthorIdField (JP.refField authorId authorIdField author)
  JP.pair author books

maybeAuthor :: JP.JsonPlan ref T.Text (Maybe Author)
maybeAuthor =
  JP.findMaybeOne authorTable authorNameField JP.rootParam

authorOverview :: JP.JsonPlan ref T.Text (Author, [Book], [Author])
authorOverview = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  laterBooks <-
    JP.findAllWhere
      bookTable
      bookAuthorIdField
      (O.fieldGreaterThan bookIdField 10)
      (JP.refField authorId authorIdField author)
  otherAuthors <- JP.selectWhere authorTable (O.fieldGreaterThan authorIdField 1)
  JP.result
    ( (,,)
        <$> JP.refValue author
        <*> JP.refValue laterBooks
        <*> JP.refValue otherAuthors
    )

authorLibrary :: JP.JsonPlan ref T.Text [(Book, Author)]
authorLibrary = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  JP.findAllEach bookTable bookAuthorIdField (JP.refField authorId authorIdField author) $
    \book -> JP.do
      bookAuthor <-
        JP.findOne authorTable authorIdField (JP.refField bookAuthorId bookAuthorIdField book)
      JP.result ((,) <$> JP.refValue book <*> JP.refValue bookAuthor)

data AuthorReportRow = AuthorReportRow
  { reportRowName :: T.Text
  , reportRowBookCount :: Int.Int32
  , reportRowTitles :: T.Text
  }
  deriving (Eq, Show)

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

authorReportPlan :: JP.JsonPlan ref T.Text AuthorReportRow
authorReportPlan = JP.do
  author <- JP.findOne authorTable authorNameField JP.rootParam
  JP.flat
    ( AuthorReportRow
        <$> JP.refCol authorName authorNameField author
        <*> JP.aggCol bookTable bookAuthorIdField (JP.refField authorId authorIdField author) countBooks
        <*> JP.aggCol bookTable bookAuthorIdField (JP.refField authorId authorIdField author) joinedTitles
    )

authorBookCount :: JP.JsonQuery T.Text (T.Text, Int)
authorBookCount =
  Profunctor.dimap
    T.strip
    (\(author, books) -> (authorName author, length books))
    (JP.jsonQuery authorWithBooks)

main :: IO ()
main = do
  mbConnString <- Env.lookupEnv "JSONPLAN_CONN"
  let
    connString =
      Maybe.fromMaybe
        "host=localhost port=15432 user=orville_test password=orville"
        mbConnString
  pool <- O.createConnectionPool (connectionOptions connString)
  O.runOrville pool $ do
    resetSchema
    seedData
    runDemo

connectionOptions :: String -> O.ConnectionOptions
connectionOptions connString =
  O.ConnectionOptions
    { O.connectionString = connString
    , O.connectionNoticeReporting = O.DisableNoticeReporting
    , O.connectionPoolStripes = O.OneStripePerCapability
    , O.connectionPoolLingerTime = 10
    , O.connectionPoolMaxConnections = O.MaxConnectionsTotal 4
    }

resetSchema :: O.MonadOrville m => m ()
resetSchema = do
  O.executeVoid Exec.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS jsonplan_book")
  O.executeVoid Exec.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS jsonplan_author")
  O.executeVoid Exec.DDLQuery $
    RawSql.fromString "CREATE TABLE jsonplan_author (id integer PRIMARY KEY, name text NOT NULL)"
  O.executeVoid Exec.DDLQuery $
    RawSql.fromString
      "CREATE TABLE jsonplan_book (id integer PRIMARY KEY, author_id integer NOT NULL, title text NOT NULL)"

seedData :: O.MonadOrville m => m ()
seedData = do
  O.insertEntities O.InOneStatement authorTable $
      Author 1 (T.pack "Ann")
        :| [ Author 2 (T.pack "Bob")
           , Author 3 (T.pack "Cid")
           ]
  O.insertEntities O.InOneStatement bookTable $
    Book 10 1 (T.pack "Ann's First")
      :| [ Book 11 1 (T.pack "Ann's Second")
         , Book 12 3 (T.pack "Cid's Only")
         ]

runDemo :: O.MonadOrville m => m ()
runDemo = do
  let
    names = fmap T.pack ["Ann", "Bob", "Cid"]
    nonEmptyNames = T.pack "Ann" :| fmap T.pack ["Bob", "Cid"]

  MIO.liftIO (putStrLn "--- native plan explain (batched) ---")
  MIO.liftIO . mapM_ putStrLn $ Plan.explain (Plan.planList (JP.toPlan authorWithBooks))

  MIO.liftIO (putStrLn "\n--- compiled single query ---")
  MIO.liftIO (putStrLn (JP.compiledSqlText authorWithBooks nonEmptyNames))

  nativeResults <- Plan.execute (Plan.planList (JP.toPlan authorWithBooks)) names
  compiledResults <- JP.executeJsonPlanList authorWithBooks names

  nativeSingle <- Plan.execute (JP.toPlan authorWithBooks) (T.pack "Ann")
  compiledSingle <- JP.executeJsonPlan authorWithBooks (T.pack "Ann")

  let maybeNames = fmap T.pack ["Ann", "Dee"]
  nativeMaybes <- Plan.execute (Plan.planList (JP.toPlan maybeAuthor)) maybeNames
  compiledMaybes <- JP.executeJsonPlanList maybeAuthor maybeNames

  let paddedNames = fmap T.pack ["  Ann", "Bob  ", " Cid "]
  nativeCounts <- Plan.execute (Plan.planList (JP.jsonQueryToPlan authorBookCount)) paddedNames
  compiledCounts <- JP.executeJsonQueryList authorBookCount paddedNames

  let overviewNames = fmap T.pack ["Ann", "Cid"]
  nativeOverviews <- Plan.execute (Plan.planList (JP.toPlan authorOverview)) overviewNames
  compiledOverviews <- JP.executeJsonPlanList authorOverview overviewNames

  nativeLibraries <- Plan.execute (Plan.planList (JP.toPlan authorLibrary)) names
  compiledLibraries <- JP.executeJsonPlanList authorLibrary names

  MIO.liftIO (putStrLn "\n--- compiled flat report query ---")
  MIO.liftIO (putStrLn (JP.compiledSqlText authorReportPlan nonEmptyNames))
  nativeFlatReport <- Plan.execute (Plan.planList (JP.toPlan authorReportPlan)) names
  compiledFlatReport <- JP.executeJsonPlanList authorReportPlan names

  let
    normalize = fmap (\(author, books) -> (author, List.sortOn bookId books))
    normalizeOne (author, books) = (author, List.sortOn bookId books)
    batchedMatch = normalize nativeResults == normalize compiledResults
    singleMatch = normalizeOne nativeSingle == normalizeOne compiledSingle
    maybeMatch = nativeMaybes == compiledMaybes
    countMatch = nativeCounts == compiledCounts
    normalizeOverview (author, books, authors) =
      (author, List.sortOn bookId books, List.sortOn authorId authors)
    overviewMatch =
      fmap normalizeOverview nativeOverviews == fmap normalizeOverview compiledOverviews
    normalizeLibrary = List.sortOn (bookId . fst)
    libraryMatch =
      fmap normalizeLibrary nativeLibraries == fmap normalizeLibrary compiledLibraries
    flatMatch = nativeFlatReport == compiledFlatReport

  MIO.liftIO (putStrLn "\n--- results ---")
  MIO.liftIO (putStrLn ("native   (batched): " <> show (normalize nativeResults)))
  MIO.liftIO (putStrLn ("compiled (batched): " <> show (normalize compiledResults)))
  MIO.liftIO (putStrLn ("native   (single) : " <> show (normalizeOne nativeSingle)))
  MIO.liftIO (putStrLn ("compiled (single) : " <> show (normalizeOne compiledSingle)))
  MIO.liftIO (putStrLn ("native   (maybe)  : " <> show nativeMaybes))
  MIO.liftIO (putStrLn ("compiled (maybe)  : " <> show compiledMaybes))
  MIO.liftIO (putStrLn ("native   (dimap)  : " <> show nativeCounts))
  MIO.liftIO (putStrLn ("compiled (dimap)  : " <> show compiledCounts))
  MIO.liftIO (putStrLn ("native   (3-ary)  : " <> show (fmap normalizeOverview nativeOverviews)))
  MIO.liftIO (putStrLn ("compiled (3-ary)  : " <> show (fmap normalizeOverview compiledOverviews)))
  MIO.liftIO (putStrLn ("native   (each)   : " <> show (fmap normalizeLibrary nativeLibraries)))
  MIO.liftIO (putStrLn ("compiled (each)   : " <> show (fmap normalizeLibrary compiledLibraries)))
  MIO.liftIO (putStrLn ("native   (flat)   : " <> show nativeFlatReport))
  MIO.liftIO (putStrLn ("compiled (flat)   : " <> show compiledFlatReport))
  MIO.liftIO . putStrLn $
    "\nbatched match: " <> show batchedMatch
      <> ", single match: " <> show singleMatch
      <> ", maybe match: " <> show maybeMatch
      <> ", dimap match: " <> show countMatch
      <> ", 3-ary match: " <> show overviewMatch
      <> ", each match: " <> show libraryMatch
      <> ", flat match: " <> show flatMatch

  if batchedMatch
    && singleMatch
    && maybeMatch
    && countMatch
    && overviewMatch
    && libraryMatch
    && flatMatch
    then MIO.liftIO (putStrLn "PASS")
    else MIO.liftIO (putStrLn "FAIL" >> Exit.exitFailure)
