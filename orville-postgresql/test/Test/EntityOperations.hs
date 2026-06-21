module Test.EntityOperations
  ( entityOperationsTests
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.Entities.CompositeKeyEntity as CompositeKeyEntity
import qualified Test.Entities.Foo as Foo
import qualified Test.Entities.UpsertEntity as UpsertEntity
import qualified Test.Property as Property

entityOperationsTests :: Orville.ConnectionPool -> Tasty.TestTree
entityOperationsTests pool =
  Tasty.testGroup
    "EntityOperations"
    [ TastyHH.testProperty
        "insertEntity/findEntitiesBy forms a round trip"
        (prop_insertEntitiesFindEntitiesByRoundTrip pool)
    , TastyHH.testProperty
        "insertEntitiesAndReturnRowCount returns the number of affected rows"
        (prop_insertEntitiesAndReturnRowCount pool)
    , TastyHH.testProperty
        "insertEntitiesAndReturnRowCount returns the number of affected rows (batched)"
        (prop_insertEntitiesAndReturnRowCountBatched pool)
    , TastyHH.testProperty
        "insertAndReturnEntities returns the inserted entities (batched)"
        (prop_insertAndReturnEntitiesBatched pool)
    , TastyHH.testProperty
        "insertEntities/findFirstEntityBy only return 1"
        (prop_insertEntitiesFindFirstEntityByRoundTrip pool)
    , TastyHH.testProperty
        "insertEntity/findEntity forms a round trip"
        (prop_insertEntityFindEntityRoundTrip pool)
    , TastyHH.testProperty
        "insertEntity/findEntities form a round trip"
        (prop_insertEntityFindEntitiesRoundTrip pool)
    , TastyHH.testProperty
        "insertEntity/findEntities form a round trip (batched)"
        (prop_insertEntityFindEntitiesRoundTripBatched pool)
    , TastyHH.testProperty
        "insertEntity/findEntities form a round trip (composite primary key)"
        (prop_insertEntityFindEntitiesCompositeKeyRoundTrip pool)
    , TastyHH.testProperty
        "insertAndReturnEntity returns the inserted entity"
        (prop_insertAndReturnEntity pool)
    , TastyHH.testProperty
        "updateEntity updates row at the given key"
        (prop_updateEntity pool)
    , TastyHH.testProperty
        "updateEntityAndReturnRowCount returns the number of affected rows"
        (prop_updateEntityAndReturnRowCount pool)
    , TastyHH.testProperty
        "updateAndReturnEntity returns the updated entity"
        (prop_updateAndReturnEntity pool)
    , TastyHH.testProperty
        "updateEntity updates no rows when key does not match"
        (prop_updateEntity_NoMatch pool)
    , TastyHH.testProperty
        "updateAndReturnEntity returns Nothing when key does not match"
        (prop_updateAndReturnEntity_NoMatch pool)
    , TastyHH.testProperty
        "deleteEntity deletes row at the given key"
        (prop_deleteEntity pool)
    , TastyHH.testProperty
        "deleteEntityAndReturnRowCount returns the number of affected rows"
        (prop_deleteEntityAndReturnRowCount pool)
    , TastyHH.testProperty
        "deleteAndReturnEntity returns the deleted row"
        (prop_deleteAndReturnEntity pool)
    , TastyHH.testProperty
        "deleteEntity deletes no rows when key doesn't match"
        (prop_deleteEntity_NoMatch pool)
    , TastyHH.testProperty
        "deleteAndReturnEntity returns Nothing when key doesn't match"
        (prop_deleteAndReturnEntity_NoMatch pool)
    , TastyHH.testProperty
        "deleteEntities deletes all matching rows"
        (prop_deleteEntities pool)
    , TastyHH.testProperty
        "deleteEntitiesAndReturnRowCount returns the number of affected rows"
        (prop_deleteEntitiesAndReturnRowCount pool)
    , TastyHH.testProperty
        "deleteEntities does not delete non matching rows"
        (prop_deleteEntities_NoMatch pool)
    , TastyHH.testProperty
        "deleteAndReturnEntities deletes all matching rows"
        (prop_deleteAndReturnEntities pool)
    , TastyHH.testProperty
        "deleteAndReturnEntities does not delete non matching rows"
        (prop_deleteAndReturnEntities_NoMatch pool)
    , TastyHH.testProperty
        "updateFields updates all matching rows"
        (prop_updateFields pool)
    , TastyHH.testProperty
        "updateFields updates no non-matching rows"
        (prop_updateFields_NoMatch pool)
    , TastyHH.testProperty
        "updateFieldsAndReturnRowCount returns the number of affeted rows"
        (prop_updateFieldsAndReturnRowCount pool)
    , TastyHH.testProperty
        "updateFieldsAndReturnEntities returns updated rows"
        (prop_updateFieldsAndReturnEntities pool)
    , TastyHH.testProperty
        "updateFieldsAndReturnEntities returns no non-matching rows"
        (prop_updateFieldsAndReturnEntities_NoMatch pool)
    , TastyHH.testProperty
        "upsertEntity ByPrimaryKey inserts entity"
        (prop_upsertByPrimaryKeyRoundTrip pool)
    , TastyHH.testProperty
        "upsertEntity ByPrimaryKey updates existing row"
        (prop_upsertByPrimaryKeyUpdatesExistingRow pool)
    , TastyHH.testProperty
        "upsertEntity ByField updates existing row"
        (prop_upsertByFieldRoundTrip pool)
    , TastyHH.testProperty
        "upsertEntity ByConstraint updates existing row"
        (prop_upsertByConstraintRoundTrip pool)
    , TastyHH.testProperty
        "upsertEntity ByMarshaller updates existing row"
        (prop_upsertByMarshallerRoundTrip pool)
    , TastyHH.testProperty
        "upsertEntity ByConflictTargetExpr updates existing row"
        (prop_upsertByConflictTargetExprRoundTrip pool)
    , TastyHH.testProperty
        "upsertAndReturnEntity returns the upserted entity"
        (prop_upsertAndReturnEntity pool)
    , TastyHH.testProperty
        "upsertEntitiesAndReturnRowCount returns the number of affected rows"
        (prop_upsertEntitiesAndReturnRowCount pool)
    , TastyHH.testProperty
        "upsertEntitiesAndReturnRowCount returns the number of affected rows (batched)"
        (prop_upsertEntitiesAndReturnRowCountBatched pool)
    , TastyHH.testProperty
        "upsertEntities/findEntities form a round trip"
        (prop_upsertEntitiesFindEntitiesRoundTrip pool)
    , TastyHH.testProperty
        "upsertEntities/findEntities form a round trip (batched)"
        (prop_upsertEntitiesFindEntitiesRoundTripBatched pool)
    , TastyHH.testProperty
        "upsertAndReturnEntities returns the upserted entities (batched)"
        (prop_upsertAndReturnEntitiesBatched pool)
    , TastyHH.testProperty
        "upsertEntities handles a batch with both inserts and updates"
        (prop_upsertEntitiesMixedInsertsAndUpdates pool)
    , TastyHH.testProperty
        "upsertEntities handles a batch with both inserts and updates (batched)"
        (prop_upsertEntitiesMixedInsertsAndUpdatesBatched pool)
    ]

prop_insertEntitiesFindEntitiesByRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_insertEntitiesFindEntitiesByRoundTrip pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_insertEntitiesFindFirstEntityByRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_insertEntitiesFindFirstEntityByRoundTrip pool =
  HH.property $ do
    originalFoos <- HH.forAll $ Foo.generateList (Range.linear 0 10)

    HH.cover 1 (String.fromString "empty list") (null originalFoos)
    HH.cover 20 (String.fromString "non-empty list") (not (null originalFoos))

    mbRetrievedFoo <-
      Foo.withTable pool $ do
        mapM_ (Orville.insertEntities Orville.InOneStatement Foo.table) (NEL.nonEmpty originalFoos)
        Orville.findFirstEntityBy Foo.table mempty

    let
      expectedLength =
        case originalFoos of
          [] -> 0
          _ -> 1

    -- Once we add order by to 'SelectOptions' we can order by something here
    -- and assert which item is returned.
    length (Maybe.maybeToList mbRetrievedFoo) === expectedLength

prop_insertEntitiesAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_insertEntitiesAndReturnRowCount pool =
  HH.property $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 10)

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntitiesAndReturnRowCount
          Orville.InOneStatement
          Foo.table
          foos

    affectedRows === length foos

prop_insertEntitiesAndReturnRowCountBatched :: Orville.ConnectionPool -> HH.Property
prop_insertEntitiesAndReturnRowCountBatched pool =
  HH.property $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 10)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntitiesAndReturnRowCount
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          Foo.table
          foos

    affectedRows === length foos

prop_insertAndReturnEntitiesBatched :: Orville.ConnectionPool -> HH.Property
prop_insertAndReturnEntitiesBatched pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 10)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    returnedFoos <-
      Foo.withTable pool $ do
        Orville.insertAndReturnEntities
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          Foo.table
          foos

    returnedFoos === NEL.toList foos

prop_insertEntityFindEntityRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_insertEntityFindEntityRoundTrip pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate

    mbRetrievedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.findEntity Foo.table (Foo.fooId originalFoo)

    mbRetrievedFoo === Just originalFoo

prop_insertEntityFindEntitiesRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_insertEntityFindEntitiesRoundTrip pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.findEntities Foo.table (Foo.fooId <$> foos)

    List.sortOn Foo.fooId retrievedFoos === List.sortOn Foo.fooId (NEL.toList foos)

prop_insertEntityFindEntitiesRoundTripBatched :: Orville.ConnectionPool -> HH.Property
prop_insertEntityFindEntitiesRoundTripBatched pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 10)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          Foo.table
          foos
        Orville.findEntities Foo.table (Foo.fooId <$> foos)

    List.sortOn Foo.fooId retrievedFoos === List.sortOn Foo.fooId (NEL.toList foos)

prop_insertEntityFindEntitiesCompositeKeyRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_insertEntityFindEntitiesCompositeKeyRoundTrip pool =
  Property.singletonProperty $ do
    compositeKeyEntities <- HH.forAll $ CompositeKeyEntity.generateNonEmpty (Range.linear 1 5)

    retrievedCompositeKeyEntities <-
      CompositeKeyEntity.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement CompositeKeyEntity.table compositeKeyEntities
        Orville.findEntities CompositeKeyEntity.table (CompositeKeyEntity.compositeKey <$> compositeKeyEntities)

    List.sortOn CompositeKeyEntity.compositeKey retrievedCompositeKeyEntities === List.sortOn CompositeKeyEntity.compositeKey (NEL.toList compositeKeyEntities)

prop_insertAndReturnEntity :: Orville.ConnectionPool -> HH.Property
prop_insertAndReturnEntity pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate

    retrievedFoo <-
      Foo.withTable pool $ do
        Orville.insertAndReturnEntity Foo.table originalFoo

    retrievedFoo === originalFoo

prop_updateEntity :: Orville.ConnectionPool -> HH.Property
prop_updateEntity pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    returnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.updateEntity Foo.table (Foo.fooId originalFoo) newFoo
        Orville.findEntitiesBy Foo.table mempty

    returnedFoo === [newFoo]

prop_updateEntityAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_updateEntityAndReturnRowCount pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.updateEntityAndReturnRowCount Foo.table (Foo.fooId originalFoo) newFoo

    affectedRows === 1

prop_updateAndReturnEntity :: Orville.ConnectionPool -> HH.Property
prop_updateAndReturnEntity pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.updateAndReturnEntity Foo.table (Foo.fooId originalFoo) newFoo

    mbReturnedFoo === Just newFoo

prop_updateEntity_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_updateEntity_NoMatch pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    let
      mismatchFooId =
        1 + Foo.fooId originalFoo

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.updateEntity Foo.table mismatchFooId newFoo
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_updateAndReturnEntity_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_updateAndReturnEntity_NoMatch pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    let
      mismatchFooId =
        1 + Foo.fooId originalFoo

    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.updateAndReturnEntity Foo.table mismatchFooId newFoo

    mbReturnedFoo === Nothing

prop_deleteEntity :: Orville.ConnectionPool -> HH.Property
prop_deleteEntity pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    let
      withDifferentKey = Gen.filter $ (Foo.fooId originalFoo /=) . Foo.fooId
    anotherFoo <- HH.forAll . withDifferentKey $ Foo.generate

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.insertEntity Foo.table anotherFoo
        Orville.deleteEntity Foo.table (Foo.fooId originalFoo)
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [anotherFoo]

prop_deleteEntityAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_deleteEntityAndReturnRowCount pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    let
      withDifferentKey = Gen.filter $ (Foo.fooId originalFoo /=) . Foo.fooId
    anotherFoo <- HH.forAll . withDifferentKey $ Foo.generate

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.insertEntity Foo.table anotherFoo
        Orville.deleteEntityAndReturnRowCount Foo.table (Foo.fooId originalFoo)

    affectedRows === 1

prop_deleteAndReturnEntity :: Orville.ConnectionPool -> HH.Property
prop_deleteAndReturnEntity pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate
    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.deleteAndReturnEntity Foo.table (Foo.fooId originalFoo)

    mbReturnedFoo === Just originalFoo

prop_deleteEntity_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_deleteEntity_NoMatch pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate

    let
      mismatchFooId =
        1 + Foo.fooId originalFoo

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.deleteEntity Foo.table mismatchFooId
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_deleteAndReturnEntity_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_deleteAndReturnEntity_NoMatch pool =
  Property.singletonProperty $ do
    originalFoo <- HH.forAll Foo.generate

    let
      mismatchFooId =
        1 + Foo.fooId originalFoo

    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.deleteAndReturnEntity Foo.table mismatchFooId

    mbReturnedFoo === Nothing

prop_deleteEntities :: Orville.ConnectionPool -> HH.Property
prop_deleteEntities pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos

    remainingFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.deleteEntities
          Foo.table
          (Just (Orville.fieldIn Foo.fooIdField fooIds))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId remainingFoos === []

prop_deleteEntitiesAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_deleteEntitiesAndReturnRowCount pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.deleteEntitiesAndReturnRowCount
          Foo.table
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    affectedRows === length foos

prop_deleteEntities_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_deleteEntities_NoMatch pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      mismatchedId = maximum fooIds + 1

    remainingFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.deleteEntities
          Foo.table
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId remainingFoos === NEL.toList fooIds

prop_deleteAndReturnEntities :: Orville.ConnectionPool -> HH.Property
prop_deleteAndReturnEntities pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos

    deletedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.deleteAndReturnEntities
          Foo.table
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId deletedFoos === NEL.toList fooIds

prop_deleteAndReturnEntities_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_deleteAndReturnEntities_NoMatch pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      mismatchedId = maximum fooIds + 1

    deletedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.deleteAndReturnEntities
          Foo.table
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))

    fmap Foo.fooId deletedFoos === []

prop_updateFields :: Orville.ConnectionPool -> HH.Property
prop_updateFields pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.updateFields
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldIn Foo.fooIdField fooIds))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooName updatedFoos === fmap (const updatedName) (NEL.toList foos)

prop_updateFields_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_updateFields_NoMatch pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      updatedName = T.pack "Updated"
      mismatchedId = maximum fooIds + 1

    foosAfterUpdate <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.updateFields
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))
        Orville.findEntitiesBy Foo.table mempty

    List.sortOn Foo.fooId foosAfterUpdate === List.sortOn Foo.fooId (NEL.toList foos)

prop_updateFieldsAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_updateFieldsAndReturnRowCount pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      updatedName = T.pack "Updated"

    affectedRows <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.updateFieldsAndReturnRowCount
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    affectedRows === length foos

prop_updateFieldsAndReturnEntities :: Orville.ConnectionPool -> HH.Property
prop_updateFieldsAndReturnEntities pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.updateFieldsAndReturnEntities
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooName updatedFoos === fmap (const updatedName) (NEL.toList foos)

prop_updateFieldsAndReturnEntities_NoMatch :: Orville.ConnectionPool -> HH.Property
prop_updateFieldsAndReturnEntities_NoMatch pool =
  Property.singletonProperty $ do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let
      fooIds = fmap Foo.fooId foos
      updatedName = T.pack "Updated"
      mismatchedId = maximum fooIds + 1

    updatedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Orville.InOneStatement Foo.table foos
        Orville.updateFieldsAndReturnEntities
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))

    updatedFoos === []

prop_upsertByPrimaryKeyRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertByPrimaryKeyRoundTrip pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate

    retrieved <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table Orville.ByPrimaryKey entity
        Orville.findEntitiesBy UpsertEntity.table mempty

    retrieved === [entity]

prop_upsertByPrimaryKeyUpdatesExistingRow :: Orville.ConnectionPool -> HH.Property
prop_upsertByPrimaryKeyUpdatesExistingRow pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    let
      updated = entity {UpsertEntity.upsertEntityValue = newValue}

    (mbOriginal, mbUpdated) <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table Orville.ByPrimaryKey entity
        mbOrig <- Orville.findEntity UpsertEntity.table (UpsertEntity.upsertEntityId entity)
        Orville.upsertEntity UpsertEntity.table Orville.ByPrimaryKey updated
        mbUpd <- Orville.findEntity UpsertEntity.table (UpsertEntity.upsertEntityId entity)
        pure (mbOrig, mbUpd)

    mbOriginal === Just entity
    mbUpdated === Just updated

prop_upsertByFieldRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertByFieldRoundTrip pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    newId <- HH.forAll UpsertEntity.generateUpsertEntityId
    let
      conflicting =
        entity
          { UpsertEntity.upsertEntityId = newId
          , UpsertEntity.upsertEntityValue = newValue
          }

    retrieved <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table (Orville.ByField UpsertEntity.upsertEntityCodeField) entity
        Orville.upsertEntity UpsertEntity.table (Orville.ByField UpsertEntity.upsertEntityCodeField) conflicting
        Orville.findEntitiesBy UpsertEntity.table mempty

    retrieved === [conflicting]

prop_upsertByConstraintRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertByConstraintRoundTrip pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    newId <- HH.forAll UpsertEntity.generateUpsertEntityId
    let
      conflicting =
        entity
          { UpsertEntity.upsertEntityId = newId
          , UpsertEntity.upsertEntityValue = newValue
          }

    retrieved <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table (Orville.ByConstraint UpsertEntity.codeUniqueConstraint) entity
        Orville.upsertEntity UpsertEntity.table (Orville.ByConstraint UpsertEntity.codeUniqueConstraint) conflicting
        Orville.findEntitiesBy UpsertEntity.table mempty

    retrieved === [conflicting]

prop_upsertByMarshallerRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertByMarshallerRoundTrip pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    newId <- HH.forAll UpsertEntity.generateUpsertEntityId
    newCode <- HH.forAll UpsertEntity.generateUpsertEntityCode
    let
      conflicting =
        entity
          { UpsertEntity.upsertEntityId = newId
          , UpsertEntity.upsertEntityCode = newCode
          , UpsertEntity.upsertEntityValue = newValue
          }

    retrieved <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table (Orville.ByMarshaller UpsertEntity.groupMarshaller) entity
        Orville.upsertEntity UpsertEntity.table (Orville.ByMarshaller UpsertEntity.groupMarshaller) conflicting
        Orville.findEntitiesBy UpsertEntity.table mempty

    retrieved === [conflicting]

prop_upsertByConflictTargetExprRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertByConflictTargetExprRoundTrip pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    let
      updated = entity {UpsertEntity.upsertEntityValue = newValue}
      target :: Orville.ConflictTarget
      target =
        Orville.ByConflictTargetExpr
          . Expr.conflictTargetForColumnNames
          . pure
          $ Orville.fieldColumnName UpsertEntity.upsertEntityIdField

    (mbOriginal, mbUpdated) <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table target entity
        mbOrig <- Orville.findEntity UpsertEntity.table (UpsertEntity.upsertEntityId entity)
        Orville.upsertEntity UpsertEntity.table target updated
        mbUpd <- Orville.findEntity UpsertEntity.table (UpsertEntity.upsertEntityId entity)
        pure (mbOrig, mbUpd)

    mbOriginal === Just entity
    mbUpdated === Just updated

prop_upsertAndReturnEntity :: Orville.ConnectionPool -> HH.Property
prop_upsertAndReturnEntity pool =
  HH.property $ do
    entity <- HH.forAll UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    let
      updated = entity {UpsertEntity.upsertEntityValue = newValue}

    (originalReturned, updatedReturned) <-
      UpsertEntity.withTable pool $ do
        original <- Orville.upsertAndReturnEntity UpsertEntity.table Orville.ByPrimaryKey entity
        upd <- Orville.upsertAndReturnEntity UpsertEntity.table Orville.ByPrimaryKey updated
        pure (original, upd)

    originalReturned === entity
    updatedReturned === updated

prop_upsertEntitiesAndReturnRowCount :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesAndReturnRowCount pool =
  HH.property $ do
    entities <- HH.forAll $ UpsertEntity.generateNonEmpty (Range.linear 1 10)

    affectedRows <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntitiesAndReturnRowCount
          Orville.InOneStatement
          UpsertEntity.table
          Orville.ByPrimaryKey
          entities

    affectedRows === length entities

prop_upsertEntitiesAndReturnRowCountBatched :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesAndReturnRowCountBatched pool =
  HH.property $ do
    entities <- HH.forAll $ UpsertEntity.generateNonEmpty (Range.linear 1 10)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    affectedRows <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntitiesAndReturnRowCount
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          UpsertEntity.table
          Orville.ByPrimaryKey
          entities

    affectedRows === length entities

prop_upsertEntitiesFindEntitiesRoundTrip :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesFindEntitiesRoundTrip pool =
  HH.property $ do
    entities <- HH.forAll $ UpsertEntity.generateNonEmpty (Range.linear 1 5)

    retrievedEntities <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntities Orville.InOneStatement UpsertEntity.table Orville.ByPrimaryKey entities
        Orville.findEntities UpsertEntity.table (UpsertEntity.upsertEntityId <$> entities)

    List.sortOn UpsertEntity.upsertEntityId retrievedEntities === List.sortOn UpsertEntity.upsertEntityId (NEL.toList entities)

prop_upsertEntitiesFindEntitiesRoundTripBatched :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesFindEntitiesRoundTripBatched pool =
  Property.singletonProperty $ do
    entities <- HH.forAll $ UpsertEntity.generateNonEmpty (Range.linear 1 5)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    retrievedEntities <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntities
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          UpsertEntity.table
          Orville.ByPrimaryKey
          entities
        Orville.findEntities UpsertEntity.table (UpsertEntity.upsertEntityId <$> entities)

    List.sortOn UpsertEntity.upsertEntityId retrievedEntities === List.sortOn UpsertEntity.upsertEntityId (NEL.toList entities)

prop_upsertAndReturnEntitiesBatched :: Orville.ConnectionPool -> HH.Property
prop_upsertAndReturnEntitiesBatched pool =
  Property.singletonProperty $ do
    entities <- HH.forAll $ UpsertEntity.generateNonEmpty (Range.linear 1 10)
    batchSize <- HH.forAll $ Gen.int (Range.linear 1 5)

    returnedEntities <-
      UpsertEntity.withTable pool $ do
        Orville.upsertAndReturnEntities
          (Orville.InBatches (Orville.BatchSize batchSize) Orville.WithNewTransaction)
          UpsertEntity.table
          Orville.ByPrimaryKey
          entities

    List.sortOn UpsertEntity.upsertEntityId returnedEntities === List.sortOn UpsertEntity.upsertEntityId (NEL.toList entities)

prop_upsertEntitiesMixedInsertsAndUpdates :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesMixedInsertsAndUpdates pool =
  Property.singletonProperty $ do
    existing <- HH.forAll UpsertEntity.generate
    new <- HH.forAll $ Gen.filter (\e -> UpsertEntity.upsertEntityId e /= UpsertEntity.upsertEntityId existing) UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    let
      updated = existing {UpsertEntity.upsertEntityValue = newValue}

    retrievedEntities <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table Orville.ByPrimaryKey existing
        Orville.upsertEntities Orville.InOneStatement UpsertEntity.table Orville.ByPrimaryKey (updated :| [new])
        Orville.findEntitiesBy UpsertEntity.table mempty

    List.sortOn UpsertEntity.upsertEntityId retrievedEntities === List.sortOn UpsertEntity.upsertEntityId [updated, new]

prop_upsertEntitiesMixedInsertsAndUpdatesBatched :: Orville.ConnectionPool -> HH.Property
prop_upsertEntitiesMixedInsertsAndUpdatesBatched pool =
  Property.singletonProperty $ do
    existing <- HH.forAll UpsertEntity.generate
    new <- HH.forAll $ Gen.filter (\e -> UpsertEntity.upsertEntityId e /= UpsertEntity.upsertEntityId existing) UpsertEntity.generate
    newValue <- HH.forAll UpsertEntity.generateUpsertEntityValue
    let
      updated = existing {UpsertEntity.upsertEntityValue = newValue}

    retrievedEntities <-
      UpsertEntity.withTable pool $ do
        Orville.upsertEntity UpsertEntity.table Orville.ByPrimaryKey existing
        Orville.upsertEntities
          (Orville.InBatches (Orville.BatchSize 1) Orville.WithNewTransaction)
          UpsertEntity.table
          Orville.ByPrimaryKey
          (updated :| [new])
        Orville.findEntitiesBy UpsertEntity.table mempty

    List.sortOn UpsertEntity.upsertEntityId retrievedEntities === List.sortOn UpsertEntity.upsertEntityId [updated, new]
