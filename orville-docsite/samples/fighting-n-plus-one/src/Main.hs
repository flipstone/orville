-- SNIPPET: moduleHeader
{-# LANGUAGE QualifiedDo #-}
module Main
  ( main
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Plan.Syntax as PlanSyntax

import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Int as Int
import qualified Data.IORef as IORef
import qualified Data.Text as T
-- SNIPPET: tableDefinitions
------------
-- Author --
------------

type AuthorId = Int.Int32

data Author = Author
  { authorId :: AuthorId
  , authorName :: T.Text
  }

authorIdField :: O.FieldDefinition O.NotNull AuthorId
authorIdField =
  O.integerField "id"

authorNameField :: O.FieldDefinition O.NotNull T.Text
authorNameField =
  O.unboundedTextField "name"

authorMarshaller :: O.SqlMarshaller Author Author
authorMarshaller =
  Author
    <$> O.marshallField authorId authorIdField
    <*> O.marshallField authorName authorNameField

authorTable :: O.TableDefinition (O.HasKey AuthorId) Author Author
authorTable =
  O.mkTableDefinition "n_plus_one_author" (O.primaryKey authorIdField) authorMarshaller

----------
-- Book --
----------

type BookId = Int.Int32

data Book = Book
  { bookId :: BookId
  , bookTitle :: T.Text
  , bookAuthorId :: AuthorId
  }

bookIdField :: O.FieldDefinition O.NotNull BookId
bookIdField =
  O.integerField "id"

bookTitleField :: O.FieldDefinition O.NotNull T.Text
bookTitleField =
  O.unboundedTextField "title"

bookAuthorIdField :: O.FieldDefinition O.NotNull AuthorId
bookAuthorIdField =
  O.integerField "author_id"

bookMarshaller :: O.SqlMarshaller Book Book
bookMarshaller =
  Book
    <$> O.marshallField bookId bookIdField
    <*> O.marshallField bookTitle bookTitleField
    <*> O.marshallField bookAuthorId bookAuthorIdField

bookTable :: O.TableDefinition (O.HasKey BookId) Book Book
bookTable =
  O.addTableConstraints
    [ O.foreignKeyConstraint (O.tableIdentifier authorTable) $
        O.foreignReference (O.fieldName bookAuthorIdField) (O.fieldName authorIdField) :| []
    ]
  $ O.mkTableDefinition "n_plus_one_book" (O.primaryKey bookIdField) bookMarshaller

------------
-- Review --
------------

data Review = Review
  { reviewId :: Int.Int32
  , reviewBookId :: BookId
  , reviewStars :: Int.Int32
  }

reviewIdField :: O.FieldDefinition O.NotNull Int.Int32
reviewIdField =
  O.integerField "id"

reviewBookIdField :: O.FieldDefinition O.NotNull BookId
reviewBookIdField =
  O.integerField "book_id"

reviewStarsField :: O.FieldDefinition O.NotNull Int.Int32
reviewStarsField =
  O.integerField "stars"

reviewMarshaller :: O.SqlMarshaller Review Review
reviewMarshaller =
  Review
    <$> O.marshallField reviewId reviewIdField
    <*> O.marshallField reviewBookId reviewBookIdField
    <*> O.marshallField reviewStars reviewStarsField

reviewTable :: O.TableDefinition (O.HasKey Int.Int32) Review Review
reviewTable =
  O.addTableConstraints
    [ O.foreignKeyConstraint (O.tableIdentifier bookTable) $
        O.foreignReference (O.fieldName reviewBookIdField) (O.fieldName bookIdField) :| []
    ]
  $ O.mkTableDefinition "n_plus_one_review" (O.primaryKey reviewIdField) reviewMarshaller
-- SNIPPET: countSelectQueries
countSelectQueries :: O.Orville a -> O.Orville (a, Int)
countSelectQueries action = do
  counter <- liftIO (IORef.newIORef 0)

  let
    countSelects queryType _sql runQuery = do
      case queryType of
        O.SelectQuery -> IORef.modifyIORef' counter (+ 1)
        _ -> pure ()
      runQuery

  result <- O.localOrvilleState (O.addSqlExecutionCallback countSelects) action
  count <- liftIO (IORef.readIORef counter)
  pure (result, count)
-- SNIPPET: naiveAuthorNames
booksInOrder :: O.SelectOptions
booksInOrder =
  O.orderBy (O.orderByField bookIdField O.ascendingOrder)

naiveAuthorNames :: O.Orville [T.Text]
naiveAuthorNames = do
  books <- O.findEntitiesBy bookTable booksInOrder
  forM books $ \book -> do
    maybeAuthor <- O.findEntity authorTable (bookAuthorId book)
    pure (maybe (T.pack "unknown") authorName maybeAuthor)
-- SNIPPET: authorOfBookPlan
authorOfBookPlan :: Plan.Plan scope Book Author
authorOfBookPlan =
  Plan.focusParam bookAuthorId (Plan.findOne authorTable authorIdField)

plannedAuthorNames :: O.Orville [T.Text]
plannedAuthorNames = do
  books <- O.findEntitiesBy bookTable booksInOrder
  authors <- Plan.execute (Plan.planList authorOfBookPlan) books
  pure (fmap authorName authors)
-- SNIPPET: bookDetailsPlan
data BookDetails = BookDetails
  { detailsBook :: Book
  , detailsAuthor :: Author
  , detailsReviews :: [Review]
  }

bookDetailsPlan :: Plan.Plan scope Book BookDetails
bookDetailsPlan = PlanSyntax.do
  book <- Plan.askParam
  author <- authorOfBookPlan
  reviews <- Plan.focusParam bookId (Plan.findAll reviewTable reviewBookIdField)
  BookDetails
    <$> Plan.use book
    <*> Plan.use author
    <*> Plan.use reviews
-- SNIPPET: authorBibliographyPlan
authorBibliographyPlan :: Plan.Plan scope AuthorId (Author, [BookDetails])
authorBibliographyPlan = PlanSyntax.do
  author <- Plan.findOne authorTable authorIdField
  books <- Plan.findAll bookTable bookAuthorIdField
  details <- Plan.using books (Plan.planList bookDetailsPlan)
  (,) <$> Plan.use author <*> Plan.use details
-- SNIPPET: describeBook
describeBook :: BookDetails -> String
describeBook details =
  concat
    [ T.unpack (bookTitle (detailsBook details))
    , " by "
    , T.unpack (authorName (detailsAuthor details))
    , ", "
    , show (length (detailsReviews details))
    , " review(s)"
    ]
-- SNIPPET: mainFunction
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  O.runOrville pool $ do
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions
      [ AutoMigration.SchemaTable authorTable
      , AutoMigration.SchemaTable bookTable
      , AutoMigration.SchemaTable reviewTable
      ]
    O.deleteEntities reviewTable Nothing
    O.deleteEntities bookTable Nothing
    O.deleteEntities authorTable Nothing
    mapM_ (O.insertEntity authorTable)
      [ Author 1 (T.pack "Fyodor Dostoyevsky")
      , Author 2 (T.pack "Frank Herbert")
      ]
    mapM_ (O.insertEntity bookTable)
      [ Book 1 (T.pack "Brothers Karamazov") 1
      , Book 2 (T.pack "The Idiot") 1
      , Book 3 (T.pack "Crime and Punishment") 1
      , Book 4 (T.pack "Dune") 2
      , Book 5 (T.pack "God Emperor of Dune") 2
      ]
    mapM_ (O.insertEntity reviewTable)
      [ Review 1 1 5
      , Review 2 1 4
      , Review 3 2 5
      , Review 4 4 5
      , Review 5 4 5
      , Review 6 4 3
      ]
-- SNIPPET: runNaiveAuthorNames
  (naiveNames, naiveCount) <- O.runOrville pool (countSelectQueries naiveAuthorNames)
  print naiveNames
  putStrLn ("Loop: " <> show naiveCount <> " SELECT queries")
-- SNIPPET: runPlannedAuthorNames
  (plannedNames, plannedCount) <- O.runOrville pool (countSelectQueries plannedAuthorNames)
  print plannedNames
  putStrLn ("Plan: " <> show plannedCount <> " SELECT queries")
-- SNIPPET: explainBookDetailsPlan
  mapM_ putStrLn (Plan.explain bookDetailsPlan)
  putStrLn "---"
  mapM_ putStrLn (Plan.explain (Plan.planList bookDetailsPlan))
-- SNIPPET: runAuthorBibliographyPlan
  (bibliographies, bibliographyCount) <-
    O.runOrville pool . countSelectQueries $
      Plan.execute (Plan.planList authorBibliographyPlan) [1, 2]

  forM_ bibliographies $ \(author, details) -> do
    putStrLn (T.unpack (authorName author))
    forM_ (sortOn (bookId . detailsBook) details) $ \detail ->
      putStrLn ("  " <> describeBook detail)
  putStrLn ("Bibliographies: " <> show bibliographyCount <> " SELECT queries")
