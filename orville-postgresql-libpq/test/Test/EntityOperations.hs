module Test.EntityOperations
  ( entityOperationsTests,
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Connection

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

entityOperationsTests :: Pool.Pool Connection.Connection -> Property.Group
entityOperationsTests pool =
  Property.group "EntityOperations" $
    [ prop_insertEntitiesFindEntitiesByRoundTrip pool
    , prop_insertEntitiesFindFirstEntityByRoundTrip pool
    , prop_insertEntityFindEntityRoundTrip pool
    , prop_insertAndReturnEntity pool
    , prop_updateEntity pool
    , prop_updateAndReturnEntity pool
    , prop_updateEntity_NoMatch pool
    , prop_updateAndReturnEntity_NoMatch pool
    , prop_deleteEntity pool
    , prop_deleteAndReturnEntity pool
    , prop_deleteEntity_NoMatch pool
    , prop_deleteAndReturnEntity_NoMatch pool
    , prop_updateFields pool
    , prop_updateFields_NoMatch pool
    , prop_updateFieldsAndReturnEntities pool
    , prop_updateFieldsAndReturnEntities_NoMatch pool
    ]

prop_insertEntitiesFindEntitiesByRoundTrip :: Property.NamedDBProperty
prop_insertEntitiesFindEntitiesByRoundTrip =
  Property.singletonNamedDBProperty "insertEntity/findEntitiesBy forms a round trip" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    retrievedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
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

prop_insertEntityFindEntityRoundTrip :: Property.NamedDBProperty
prop_insertEntityFindEntityRoundTrip =
  Property.singletonNamedDBProperty "insertEntity/findEntity forms a round trip" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate

    mbRetrievedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
        Orville.findEntity Foo.table (Foo.fooId originalFoo)

    mbRetrievedFoo === Just originalFoo

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
        Orville.insertEntity Foo.table originalFoo
        Orville.updateEntity Foo.table (Foo.fooId originalFoo) newFoo
        Orville.findEntitiesBy Foo.table mempty

    returnedFoo === [newFoo]

prop_updateAndReturnEntity :: Property.NamedDBProperty
prop_updateAndReturnEntity =
  Property.singletonNamedDBProperty "updateAndReturnEntity returns the updated entity" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    newFoo <- HH.forAll Foo.generate

    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
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
        Orville.insertEntity Foo.table originalFoo
        Orville.updateEntity Foo.table mismatchFooId newFoo
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
        Orville.insertEntity Foo.table originalFoo
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
        Orville.insertEntity Foo.table originalFoo
        Orville.insertEntity Foo.table anotherFoo
        Orville.deleteEntity Foo.table (Foo.fooId originalFoo)
        Orville.findEntitiesBy Foo.table mempty

    retrievedFoos === [anotherFoo]

prop_deleteAndReturnEntity :: Property.NamedDBProperty
prop_deleteAndReturnEntity =
  Property.singletonNamedDBProperty "deleteAndReturnEntity returns the deleted row" $ \pool -> do
    originalFoo <- HH.forAll Foo.generate
    mbReturnedFoo <-
      Foo.withTable pool $ do
        Orville.insertEntity Foo.table originalFoo
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
        Orville.insertEntity Foo.table originalFoo
        Orville.deleteEntity Foo.table mismatchFooId
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
        Orville.insertEntity Foo.table originalFoo
        Orville.deleteAndReturnEntity Foo.table mismatchFooId

    mbReturnedFoo === Nothing

prop_updateFields :: Property.NamedDBProperty
prop_updateFields =
  Property.singletonNamedDBProperty "updateFields updates all matching rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Foo.table foos
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
        Orville.insertEntities Foo.table foos
        Orville.updateFields
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))
        Orville.findEntitiesBy Foo.table mempty

    List.sortOn Foo.fooId foosAfterUpdate === List.sortOn Foo.fooId (NEL.toList foos)

prop_updateFieldsAndReturnEntities :: Property.NamedDBProperty
prop_updateFieldsAndReturnEntities =
  Property.singletonNamedDBProperty "updateFieldsAndReturnEntities returns updated rows" $ \pool -> do
    foos <- HH.forAll $ Foo.generateNonEmpty (Range.linear 1 5)

    let fooIds = fmap Foo.fooId foos
        updatedName = T.pack "Updated"

    updatedFoos <-
      Foo.withTable pool $ do
        Orville.insertEntities Foo.table foos
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
        Orville.insertEntities Foo.table foos
        Orville.updateFieldsAndReturnEntities
          Foo.table
          (Orville.setField Foo.fooNameField updatedName :| [])
          (Just (Orville.fieldEquals Foo.fooIdField mismatchedId))

    updatedFoos === []
