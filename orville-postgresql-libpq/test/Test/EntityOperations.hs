module Test.EntityOperations
  ( entityOperationsTests,
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

import qualified Orville.PostgreSQL as Orville

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

entityOperationsTests :: Orville.Pool Orville.Connection -> Property.Group
entityOperationsTests pool =
  Property.group "EntityOperations" $
    [ prop_insertEntitiesFindEntitiesByRoundTrip pool
    , prop_insertEntitiesAffectedRows pool
    , prop_insertEntitiesFindFirstEntityByRoundTrip pool
    , prop_insertEntityFindEntityRoundTrip pool
    , prop_insertEntityFindEntitiesRoundTrip pool
    , prop_insertAndReturnEntity pool
    , prop_updateEntity pool
    , prop_updateEntityAffectedRows pool
    , prop_updateAndReturnEntity pool
    , prop_updateEntity_NoMatch pool
    , prop_updateAndReturnEntity_NoMatch pool
    , prop_deleteEntity pool
    , prop_deleteEntityAffectedRows pool
    , prop_deleteAndReturnEntity pool
    , prop_deleteEntity_NoMatch pool
    , prop_deleteAndReturnEntity_NoMatch pool
    , prop_deleteEntities pool
    , prop_deleteEntitiesAffectedRows pool
    , prop_deleteEntities_NoMatch pool
    , prop_deleteAndReturnEntities pool
    , prop_deleteAndReturnEntities_NoMatch pool
    , prop_updateFields pool
    , prop_updateFields_NoMatch pool
    , prop_updateFieldsAffectedRows pool
    , prop_updateFieldsAndReturnEntities pool
    , prop_updateFieldsAndReturnEntities_NoMatch pool
    ]

prop_insertEntitiesFindEntitiesByRoundTrip :: Property.NamedDBProperty
prop_insertEntitiesFindEntitiesByRoundTrip =
  Property.singletonNamedDBProperty "insertEntity/findEntitiesBy forms a round trip" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    retrievedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_insertEntitiesFindFirstEntityByRoundTrip :: Property.NamedDBProperty
prop_insertEntitiesFindFirstEntityByRoundTrip =
  Property.namedDBProperty "insertEntities/findFirstEntityBy only return 1" $ \pool -> do
    originalFoos <- HH.forAll $ Foo.generateList (Range.linear 0 10)

    HH.cover 1 (String.fromString "empty list") (null originalFoos)
    HH.cover 20 (String.fromString "non-empty list") (not (null originalFoos))

    mbRetrievedFoo <-
      Foo.withTable pool $ do
        mapM_ (Orville.insertEntities Foo.table) (NEL.nonEmpty originalFoos)
        Orville.findFirstEntityBy Foo.table mempty

    let expectedLength =
          case originalFoos of
            [] -> 0
            _ -> 1

    -- Once we add order by to 'SelectOptions' we can order by something here
    -- and assert which item is returned.
    length (Maybe.maybeToList mbRetrievedFoo) === expectedLength

prop_insertEntitiesAffectedRows :: Property.NamedDBProperty
prop_insertEntitiesAffectedRows =
  Property.namedDBProperty "insertEntities returns the number of affected rows" $ \pool -> do
    originalFoos <- HH.forAll $ Foo.generateList (Range.linear 0 10)

    HH.cover 1 (String.fromString "empty list") (null originalFoos)
    HH.cover 20 (String.fromString "non-empty list") (not (null originalFoos))

    affectedRows <-
      Foo.withTable pool $ do
        case NEL.nonEmpty originalFoos of
          Nothing -> pure 0
          Just nonEmptyFoos -> Orville.insertEntities Foo.table nonEmptyFoos

    affectedRows === length originalFoos

prop_insertEntityFindEntityRoundTrip :: Property.NamedDBProperty
prop_insertEntityFindEntityRoundTrip =
  Property.singletonNamedDBProperty "insertEntity/findEntity forms a round trip" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    mbRetrievedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.findEntity Foo.table (Foo.fooId originalFoo)

    mbRetrievedFoo === Just originalFoo

prop_insertEntityFindEntitiesRoundTrip :: Property.NamedDBProperty
prop_insertEntityFindEntitiesRoundTrip =
  Property.singletonNamedDBProperty "insertEntity/findEntities form a round trip" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    retrievedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.findEntities Foo.table (Foo.fooId <$> foos)

    List.sortOn Foo.fooId retrievedFoos === List.sortOn Foo.fooId (NEL.toList foos)

prop_insertAndReturnEntity :: Property.NamedDBProperty
prop_insertAndReturnEntity =
  Property.singletonNamedDBProperty "insertAndReturnEntity returns the inserted entity" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    retrievedFoo <-
      Foo.withTable pool $ do
        Orville.insertAndReturnEntity Foo.table originalFoo

    retrievedFoo === originalFoo

prop_updateEntity :: Property.NamedDBProperty
prop_updateEntity =
  Property.singletonNamedDBProperty "updateEntity updates row at the given key" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    returnedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        _ <- Orville.updateEntity Foo.table (Foo.fooId originalFoo) newFoo
        Orville.findEntitiesBy Foo.table mempty

    returnedFoo === [newFoo]

prop_updateEntityAffectedRows :: Property.NamedDBProperty
prop_updateEntityAffectedRows =
  Property.singletonNamedDBProperty "updateEntity returns the number of affected rows" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    affectedRows <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.updateEntity Foo.table (Foo.fooId originalFoo) newFoo

    affectedRows === 1

prop_updateAndReturnEntity :: Property.NamedDBProperty
prop_updateAndReturnEntity =
  Property.singletonNamedDBProperty "updateAndReturnEntity returns the updated entity" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    mbReturnedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.updateAndReturnEntity Foo.table (Foo.fooId originalFoo) newFoo

    mbReturnedFoo === Just newFoo

prop_updateEntity_NoMatch :: Property.NamedDBProperty
prop_updateEntity_NoMatch =
  Property.singletonNamedDBProperty "updateEntity updates no rows when key does not match" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    let mismatchFooId =
          1 + Foo.fooId originalFoo

    retrievedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        _ <- Orville.updateEntity Foo.table mismatchFooId newFoo
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_updateAndReturnEntity_NoMatch :: Property.NamedDBProperty
prop_updateAndReturnEntity_NoMatch =
  Property.singletonNamedDBProperty "updateAndReturnEntity returns Nothing when key does not match" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    let mismatchFooId =
          1 + Foo.fooId originalFoo

    mbReturnedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.updateAndReturnEntity Foo.table mismatchFooId newFoo

    mbReturnedFoo === Nothing

prop_deleteEntity :: Property.NamedDBProperty
prop_deleteEntity =
  Property.singletonNamedDBProperty "deleteEntity deletes row at the given key" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    let withDifferentKey = Gen.filter $ (Foo.fooId originalFoo /=) . Foo.fooId
    anotherFoo <- HH.forAll . withDifferentKey $ Foo.generate

    retrievedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        _ <- Orville.insertEntity Foo.table anotherFoo
        _ <- Orville.deleteEntity Foo.table (Foo.fooId originalFoo)
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [anotherFoo]

prop_deleteEntityAffectedRows :: Property.NamedDBProperty
prop_deleteEntityAffectedRows =
  Property.singletonNamedDBProperty "deleteEntity returns the number of affected rows" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    let withDifferentKey = Gen.filter $ (Foo.fooId originalFoo /=) . Foo.fooId
    anotherFoo <- HH.forAll . withDifferentKey $ Foo.generate

    affectedRows <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        _ <- Orville.insertEntity Foo.table anotherFoo
        Orville.deleteEntity Foo.table (Foo.fooId originalFoo)

    affectedRows === 1

prop_deleteAndReturnEntity :: Property.NamedDBProperty
prop_deleteAndReturnEntity =
  Property.singletonNamedDBProperty "deleteAndReturnEntity returns the deleted row" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    mbReturnedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.deleteAndReturnEntity Foo.table (Foo.fooId originalFoo)

    mbReturnedFoo === Just originalFoo

prop_deleteEntity_NoMatch :: Property.NamedDBProperty
prop_deleteEntity_NoMatch =
  Property.singletonNamedDBProperty "deleteEntity deletes no rows when key doesn't match" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let mismatchFooId =
          1 + Foo.fooId originalFoo

    retrievedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        _ <- Orville.deleteEntity Foo.table mismatchFooId
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [originalFoo]

prop_deleteAndReturnEntity_NoMatch :: Property.NamedDBProperty
prop_deleteAndReturnEntity_NoMatch =
  Property.singletonNamedDBProperty "deleteAndReturnEntity returns Nothing when key doesn't match" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    let mismatchFooId =
          1 + Foo.fooId originalFoo

    mbReturnedFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table originalFoo
        Orville.deleteAndReturnEntity Foo.table mismatchFooId

    mbReturnedFoo === Nothing

prop_deleteEntities :: Property.NamedDBProperty
prop_deleteEntities =
  Property.singletonNamedDBProperty "deleteEntities deletes all matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos

    remainingFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        _ <-
          Orville.deleteEntities
            Foo.table
            (Just (Orville.fieldIn Foo.fooIdField fooIds))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId remainingFoos === []

prop_deleteEntitiesAffectedRows :: Property.NamedDBProperty
prop_deleteEntitiesAffectedRows =
  Property.singletonNamedDBProperty "deleteEntities returns the number of affected rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos

    affectedRows <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.deleteEntities
          Foo.table
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    affectedRows === length foos

prop_deleteEntities_NoMatch :: Property.NamedDBProperty
prop_deleteEntities_NoMatch =
  Property.singletonNamedDBProperty "deleteEntities does not delete non matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        mismatchedId = maximum fooIds + 1

    remainingFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        _ <-
          Orville.deleteEntities
            Foo.table
            (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId remainingFoos === NEL.toList fooIds

prop_deleteAndReturnEntities :: Property.NamedDBProperty
prop_deleteAndReturnEntities =
  Property.singletonNamedDBProperty "deleteAndReturnEntities deletes all matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos

    deletedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.deleteAndReturnEntities
          Foo.table
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooId deletedFoos === NEL.toList fooIds

prop_deleteAndReturnEntities_NoMatch :: Property.NamedDBProperty
prop_deleteAndReturnEntities_NoMatch =
  Property.singletonNamedDBProperty "deleteAndReturnEntities does not delete non matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        mismatchedId = maximum fooIds + 1

    deletedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.deleteAndReturnEntities
          Foo.table
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))

    fmap Foo.fooId deletedFoos === []

prop_updateFields :: Property.NamedDBProperty
prop_updateFields =
  Property.singletonNamedDBProperty "updateFields updates all matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        _ <-
          Orville.updateFields
            Foo.table
            (Orville.setField Foo.fooNameField updatedName :| [])
            (Just (Orville.fieldIn Foo.fooIdField fooIds))
        Orville.findEntitiesBy
          Foo.table
          (Orville.where_ (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooName updatedFoos === fmap (const updatedName) (NEL.toList foos)

prop_updateFields_NoMatch :: Property.NamedDBProperty
prop_updateFields_NoMatch =
  Property.singletonNamedDBProperty "updateFields updates no non-matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"
        mismatchedId = maximum fooIds + 1

    foosAfterUpdate <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        _ <-
          Orville.updateFields
            Foo.table
            (Orville.setField Foo.fooNameField updatedName :| [])
            (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))
        Orville.findEntitiesBy Foo.table mempty

    List.sortOn Foo.fooId foosAfterUpdate === List.sortOn Foo.fooId (NEL.toList foos)

prop_updateFieldsAffectedRows :: Property.NamedDBProperty
prop_updateFieldsAffectedRows =
  Property.singletonNamedDBProperty "updateFields returns the number of affeted rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"

    affectedRows <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.updateFields
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    affectedRows === length foos

prop_updateFieldsAndReturnEntities :: Property.NamedDBProperty
prop_updateFieldsAndReturnEntities =
  Property.singletonNamedDBProperty "updateFieldsAndReturnEntities returns updated rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.updateFieldsAndReturnEntities
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldIn Foo.fooIdField fooIds))

    fmap Foo.fooName updatedFoos === fmap (const updatedName) (NEL.toList foos)

prop_updateFieldsAndReturnEntities_NoMatch :: Property.NamedDBProperty
prop_updateFieldsAndReturnEntities_NoMatch =
  Property.singletonNamedDBProperty "updateFieldsAndReturnEntities returns no non-matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"
        mismatchedId = maximum fooIds + 1

    updatedFoos <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntities Foo.table foos
        Orville.updateFieldsAndReturnEntities
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))

    updatedFoos === []
