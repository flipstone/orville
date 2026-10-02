{-# LANGUAGE RankNTypes #-}

{- | Unit tests feeding hostile boundary JSON directly to plan decoders, plus
  the failure path of a compiled findOne that matches nothing.
-}
module Test.Decoders
  ( tests
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import Data.String (fromString)
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as O

import qualified Orville.JsonPlan as JP
import qualified Test.Fixtures as F

tests :: O.ConnectionPool -> HH.Group
tests pool =
  HH.Group
    (fromString "Test.Decoders")
    [ (fromString "result decoder rejects non-objects", singleton resultRejectsNonObject)
    , (fromString "result decoder reports missing keys", singleton resultReportsMissingKey)
    , (fromString "findAll decoder rejects non-arrays", singleton findAllRejectsNonArray)
    , (fromString "findAll decoder rejects non-object elements", singleton findAllRejectsNonObjectElements)
    , (fromString "entity decoder reports marshalling failures", singleton entityReportsMarshallFailure)
    , (fromString "compiled findOne miss throws NoRowMatched", singleton (findOneMissThrows pool))
    ]

singleton :: HH.PropertyT IO () -> HH.Property
singleton =
  HH.withTests 1 . HH.property

decodeValue ::
  (forall ref. JP.JsonPlan ref param result) ->
  Aeson.Value ->
  IO (Either JP.DecodeError result)
decodeValue plan =
  JP.runJsonDecoder (JP.planDecoder plan)

expectDecodeError ::
  (JP.DecodeError -> Bool) ->
  Either JP.DecodeError result ->
  HH.PropertyT IO ()
expectDecodeError matches decoded =
  case decoded of
    Left err
      | matches err ->
          HH.success
      | otherwise -> do
          HH.annotate (JP.renderDecodeError err)
          HH.failure
    Right _ -> do
      HH.annotate "decoding succeeded, but a decode error was expected"
      HH.failure

resultRejectsNonObject :: HH.PropertyT IO ()
resultRejectsNonObject = do
  decoded <- HH.evalIO (decodeValue F.authorWithBooks (Aeson.String (T.pack "nope")))
  expectDecodeError
    ( \err ->
        case err of
          JP.ExpectedJsonObject context -> context == "result"
          _ -> False
    )
    decoded

resultReportsMissingKey :: HH.PropertyT IO ()
resultReportsMissingKey = do
  decoded <- HH.evalIO (decodeValue F.authorWithBooks (Aeson.object []))
  expectDecodeError
    ( \err ->
        case err of
          JP.MissingResultKey key -> key == "r0"
          _ -> False
    )
    decoded

findAllRejectsNonArray :: HH.PropertyT IO ()
findAllRejectsNonArray = do
  decoded <- HH.evalIO (decodeValue F.booksByAuthorId Aeson.Null)
  expectDecodeError
    ( \err ->
        case err of
          JP.ExpectedJsonArray stepKind -> stepKind == "findAll"
          _ -> False
    )
    decoded

findAllRejectsNonObjectElements :: HH.PropertyT IO ()
findAllRejectsNonObjectElements = do
  decoded <- HH.evalIO (decodeValue F.booksByAuthorId (Aeson.toJSON [True]))
  expectDecodeError
    ( \err ->
        case err of
          JP.ExpectedJsonObject context -> context == "entity element"
          _ -> False
    )
    decoded

entityReportsMarshallFailure :: HH.PropertyT IO ()
entityReportsMarshallFailure = do
  decoded <- HH.evalIO (decodeValue F.maybeAuthor (Aeson.object []))
  expectDecodeError
    ( \err ->
        case err of
          JP.EntityMarshallError _ -> True
          _ -> False
    )
    decoded

findOneMissThrows :: O.ConnectionPool -> HH.PropertyT IO ()
findOneMissThrows pool = do
  let
    someAuthor = F.Author 1 (T.pack "present") (T.pack "A")
  outcome <-
    HH.evalIO . Exception.try . O.runOrville pool $ do
      F.seedDataset (F.Dataset [someAuthor] [])
      JP.executeJsonPlan F.authorWithBooks (T.pack "absent")
  case (outcome :: Either JP.JsonPlanError (F.Author, [F.Book])) of
    Left (JP.BoundaryDecodeFailed JP.NoRowMatched) ->
      HH.success
    Left err -> do
      HH.annotate (JP.renderJsonPlanError err)
      HH.failure
    Right _ -> do
      HH.annotate "execution succeeded, but a NoRowMatched failure was expected"
      HH.failure
