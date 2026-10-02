{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

{- | Unit tests for the compile-time pre-checks: boundary shape rejection for
  field projections and wire-text rendering rejection for unknown type oids.
  These are pure and need no database.
-}
module Test.CompileChecks
  ( tests
  ) where

import qualified Data.Either as Either
import qualified Data.Int as Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hedgehog as HH
import Hedgehog ((===))

import qualified Orville.PostgreSQL as O

import qualified Orville.JsonPlan as JP
import qualified Test.Fixtures as F

tests :: HH.Group
tests =
  HH.Group
    (fromString "Test.CompileChecks")
    [ (fromString "the fixture plans compile", singleton fixturePlansCompile)
    , (fromString "rejects refField on a findAll reference", singleton rejectsFindAllRef)
    , (fromString "rejects refField on a reference rebound through use", singleton rejectsReboundListRef)
    , (fromString "accepts refField on a findOne reference rebound through use", singleton acceptsReboundEntityRef)
    , (fromString "rejects refField on a findMaybeOne reference", singleton rejectsMaybeRef)
    , (fromString "rejects a column type with an unknown oid", singleton rejectsUnknownOid)
    ]

singleton :: HH.PropertyT IO () -> HH.Property
singleton =
  HH.withTests 1 . HH.property

textParams :: NEL.NonEmpty T.Text
textParams =
  T.pack "x" :| []

expectCompileError ::
  (JP.JsonPlanError -> Bool) ->
  Either JP.JsonPlanError String ->
  HH.PropertyT IO ()
expectCompileError matches compileResult =
  case compileResult of
    Left err
      | matches err ->
          HH.success
      | otherwise -> do
          HH.annotate (JP.renderJsonPlanError err)
          HH.failure
    Right _ -> do
      HH.annotate "plan compiled, but a compile error was expected"
      HH.failure

fixturePlansCompile :: HH.PropertyT IO ()
fixturePlansCompile = do
  Either.isRight (JP.compiledSqlText F.authorWithBooks textParams) === True
  Either.isRight (JP.compiledSqlText F.maybeAuthor textParams) === True
  Either.isRight (JP.compiledSqlText F.authorLibrary textParams) === True
  Either.isRight (JP.compiledSqlText F.flatReport textParams) === True
  Either.isRight (JP.compiledSqlText F.booksByAuthorId (1 :| [])) === True

firstBookAuthorId :: [F.Book] -> Int.Int32
firstBookAuthorId =
  maybe 0 F.bookAuthorId . Maybe.listToMaybe

refFieldOnFindAll :: JP.JsonPlan ref T.Text F.Author
refFieldOnFindAll = JP.do
  author <- JP.findOne F.authorTable F.authorNameField JP.rootParam
  books <- JP.findAll F.bookTable F.bookAuthorIdField (JP.refField F.authorId F.authorIdField author)
  JP.findOne F.authorTable F.authorIdField (JP.refField firstBookAuthorId F.authorIdField books)

rejectsFindAllRef :: HH.PropertyT IO ()
rejectsFindAllRef =
  expectCompileError
    (isRefFieldOn "findAll")
    (JP.compiledSqlText refFieldOnFindAll textParams)

refFieldOnReboundList :: JP.JsonPlan ref T.Text F.Author
refFieldOnReboundList = JP.do
  author <- JP.findOne F.authorTable F.authorNameField JP.rootParam
  books <- JP.findAll F.bookTable F.bookAuthorIdField (JP.refField F.authorId F.authorIdField author)
  booksAgain <- JP.use books
  JP.findOne F.authorTable F.authorIdField (JP.refField firstBookAuthorId F.authorIdField booksAgain)

rejectsReboundListRef :: HH.PropertyT IO ()
rejectsReboundListRef =
  expectCompileError
    (isRefFieldOn "findAll")
    (JP.compiledSqlText refFieldOnReboundList textParams)

refFieldOnReboundEntity :: JP.JsonPlan ref T.Text [F.Book]
refFieldOnReboundEntity = JP.do
  author <- JP.findOne F.authorTable F.authorNameField JP.rootParam
  authorAgain <- JP.use author
  JP.findAll F.bookTable F.bookAuthorIdField (JP.refField F.authorId F.authorIdField authorAgain)

acceptsReboundEntityRef :: HH.PropertyT IO ()
acceptsReboundEntityRef =
  Either.isRight (JP.compiledSqlText refFieldOnReboundEntity textParams) === True

refFieldOnMaybe :: JP.JsonPlan ref T.Text [F.Author]
refFieldOnMaybe = JP.do
  found <- JP.findMaybeOne F.authorTable F.authorNameField JP.rootParam
  JP.findAll F.authorTable F.authorIdField (JP.refField (maybe 0 F.authorId) F.authorIdField found)

rejectsMaybeRef :: HH.PropertyT IO ()
rejectsMaybeRef =
  expectCompileError
    (isRefFieldOn "findMaybeOne")
    (JP.compiledSqlText refFieldOnMaybe textParams)

isRefFieldOn :: String -> JP.JsonPlanError -> Bool
isRefFieldOn expectedKind err =
  case err of
    JP.RefFieldOnNonEntity stepKind _ -> stepKind == expectedKind
    _ -> False

weirdField :: O.FieldDefinition O.NotNull T.Text
weirdField =
  O.fieldOfType
    O.unboundedText {O.sqlTypeOid = LibPQ.Oid 987654}
    "weird"

weirdTable :: O.TableDefinition (O.HasKey Int.Int32) (Int.Int32, T.Text) (Int.Int32, T.Text)
weirdTable =
  O.mkTableDefinition
    "jpt_weird"
    (O.primaryKey (O.integerField "id"))
    ( (,)
        <$> O.marshallField fst (O.integerField "id")
        <*> O.marshallField snd weirdField
    )

weirdPlan :: JP.JsonPlan ref Int.Int32 (Int.Int32, T.Text)
weirdPlan =
  JP.findOne weirdTable (O.integerField "id") JP.rootParam

rejectsUnknownOid :: HH.PropertyT IO ()
rejectsUnknownOid =
  expectCompileError
    isUnrenderableWeird
    (JP.compiledSqlText weirdPlan (1 :| []))

isUnrenderableWeird :: JP.JsonPlanError -> Bool
isUnrenderableWeird err =
  case err of
    JP.ColumnNotWireRenderable _ columnName -> columnName == "weird"
    _ -> False
