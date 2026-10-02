{-# LANGUAGE QualifiedDo #-}

module Main
  ( main
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Int as Int
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Maybe as Maybe
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

  liftIO (putStrLn "--- native plan explain (batched) ---")
  liftIO . mapM_ putStrLn $ Plan.explain (Plan.planList (JP.toPlan authorWithBooks))

  liftIO (putStrLn "\n--- compiled single query ---")
  liftIO (putStrLn (JP.compiledSqlText authorWithBooks nonEmptyNames))

  nativeResults <- Plan.execute (Plan.planList (JP.toPlan authorWithBooks)) names
  compiledResults <- JP.executeJsonPlanList authorWithBooks names

  nativeSingle <- Plan.execute (JP.toPlan authorWithBooks) (T.pack "Ann")
  compiledSingle <- JP.executeJsonPlan authorWithBooks (T.pack "Ann")

  let
    normalize = fmap (\(author, books) -> (author, List.sortOn bookId books))
    normalizeOne (author, books) = (author, List.sortOn bookId books)
    batchedMatch = normalize nativeResults == normalize compiledResults
    singleMatch = normalizeOne nativeSingle == normalizeOne compiledSingle

  liftIO (putStrLn "\n--- results ---")
  liftIO (putStrLn ("native   (batched): " <> show (normalize nativeResults)))
  liftIO (putStrLn ("compiled (batched): " <> show (normalize compiledResults)))
  liftIO (putStrLn ("native   (single) : " <> show (normalizeOne nativeSingle)))
  liftIO (putStrLn ("compiled (single) : " <> show (normalizeOne compiledSingle)))
  liftIO . putStrLn $
    "\nbatched match: " <> show batchedMatch <> ", single match: " <> show singleMatch

  if batchedMatch && singleMatch
    then liftIO (putStrLn "PASS")
    else liftIO (putStrLn "FAIL" >> Exit.exitFailure)
