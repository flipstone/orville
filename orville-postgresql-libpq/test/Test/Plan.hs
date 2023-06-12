{-# LANGUAGE CPP #-}

#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
{-# LANGUAGE QualifiedDo #-}
#endif
#endif
module Test.Plan
  ( planTests,
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Either as Either
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Plan.Many as Many

#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
import qualified Orville.PostgreSQL.Plan.Syntax as PlanSyntax
#endif
#endif

import qualified Test.Entities.Foo as Foo
import qualified Test.Entities.FooChild as FooChild
import qualified Test.Property as Property

{- ORMOLU_DISABLE -}
{- disable formatting so fourmolu doesn't go haywire with the cpp within the list -}
planTests :: Orville.Pool Orville.Connection -> Property.Group
planTests pool =
  Property.group "Plan" $
    [ prop_askParam pool
    , prop_findMaybeOne pool
    , prop_findMaybeOneWhere pool
    , prop_findAll pool
    , prop_findAllWhere pool
    , prop_planMany_findMaybeOne pool
    , prop_planMany_findMaybeOneWhere pool
    , prop_planMany_findAll pool
    , prop_planMany_findAllWhere pool
    , prop_planEither pool
    , prop_planMany_planEither pool
    , prop_bindAndUse pool
    , prop_planMany_bindAndUse pool
    , prop_planMany_findOne_dedupesInClauses pool
#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
    , prop_bindAndUse_qualifiedDo pool
    , prop_planMany_bindAndUse_qualifiedDo pool
#endif
#endif
    , prop_assert pool
    , prop_explain
    ]
{- ORMOLU_ENABLE -}

prop_askParam :: Property.NamedDBProperty
prop_askParam =
  Property.namedDBProperty "askParam returns the plan's parameter" $ \pool -> do
    let plan :: Plan.Plan scope Int Int
        plan = Plan.askParam

    inputParam <- HH.forAll $ Gen.integral (Range.constant minBound maxBound)
    resultParam <- MIO.liftIO $ Orville.runOrville pool (Plan.execute plan inputParam)
    resultParam === inputParam

prop_findMaybeOne :: Property.NamedDBProperty
prop_findMaybeOne =
  Property.namedDBProperty "findMaybeOne finds a row where the field matches (if any)" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName (Maybe Foo.Foo)
        plan = Plan.findMaybeOne Foo.table Foo.fooNameField

    (targetName, foos) <- HH.forAll generateSearchTargetAndSubjects
    maybeResult <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetName

    let isMatch = Foo.hasName targetName

    coverSearchResultCases isMatch foos
    assertMatchIsFromPredicate maybeResult isMatch foos

prop_planMany_findMaybeOne :: Property.NamedDBProperty
prop_planMany_findMaybeOne =
  Property.namedDBProperty "(planMany findMaybeOne) finds a row where the field matches for each input" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName (Maybe Foo.Foo))
        plan = Plan.planMany $ Plan.findMaybeOne Foo.table Foo.fooNameField

    (targetNames, foos) <- HH.forAll generateSearchTargetListAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetNames

    let isMatch foo = elem (Foo.fooName foo) targetNames

    coverSearchResultCases isMatch foos

    assertEachManyResult targetNames results $ \targetName maybeFoo ->
      assertMatchIsFromPredicate
        maybeFoo
        (\foo -> Foo.hasName targetName foo && isMatch foo)
        foos

prop_findMaybeOneWhere :: Property.NamedDBProperty
prop_findMaybeOneWhere =
  Property.namedDBProperty "findMaybeOneWhere finds a row where the field matches (if any), with the given condition" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName (Maybe Foo.Foo)
        plan =
          Plan.findMaybeOneWhere
            Foo.table
            Foo.fooNameField
            (Orville.fieldGreaterThan Foo.fooAgeField Foo.averageFooAge)

    (targetName, foos) <- HH.forAll generateSearchTargetAndSubjects
    maybeResult <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetName

    let isMatch foo = Foo.hasName targetName foo && Foo.fooAge foo > Foo.averageFooAge

    coverSearchResultCases isMatch foos
    assertMatchIsFromPredicate maybeResult isMatch foos

prop_planMany_findMaybeOneWhere :: Property.NamedDBProperty
prop_planMany_findMaybeOneWhere =
  Property.namedDBProperty "(planMany findMaybeOneWhere) finds a row where the field matches for each input, with the given condition" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName (Maybe Foo.Foo))
        plan =
          Plan.planMany $
            Plan.findMaybeOneWhere
              Foo.table
              Foo.fooNameField
              (Orville.fieldGreaterThan Foo.fooAgeField Foo.averageFooAge)

    (targetNames, foos) <- HH.forAll generateSearchTargetListAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetNames

    let isMatch foo =
          elem (Foo.fooName foo) targetNames
            && Foo.fooAge foo > Foo.averageFooAge

    coverSearchResultCases isMatch foos

    assertEachManyResult targetNames results $ \targetName maybeFoo ->
      assertMatchIsFromPredicate
        maybeFoo
        (\foo -> Foo.hasName targetName foo && isMatch foo)
        foos

prop_findAll :: Property.NamedDBProperty
prop_findAll =
  Property.namedDBProperty "findAll finds all rows where the field matches" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName [Foo.Foo]
        plan = Plan.findAll Foo.table Foo.fooNameField

    (targetName, foos) <- HH.forAll generateSearchTargetAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetName

    let isMatch = Foo.hasName targetName

    coverSearchResultCases isMatch foos
    assertAllMatchesFound Foo.fooId results isMatch foos

prop_planMany_findAll :: Property.NamedDBProperty
prop_planMany_findAll =
  Property.namedDBProperty "(planMany findAll) finds all rows where the field matches for each list of inputs" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName [Foo.Foo])
        plan = Plan.planMany (Plan.findAll Foo.table Foo.fooNameField)

    (targetNames, foos) <- HH.forAll generateSearchTargetListAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetNames

    let isMatch foo = elem (Foo.fooName foo) targetNames

    coverSearchResultCases isMatch foos
    assertEachManyResult targetNames results $ \targetName foundFoos ->
      assertAllMatchesFound Foo.fooId foundFoos (\foo -> Foo.hasName targetName foo && isMatch foo) foos

prop_findAllWhere :: Property.NamedDBProperty
prop_findAllWhere =
  Property.namedDBProperty "findAllWhere finds all rows where the field matches, with the given condition" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName [Foo.Foo]
        plan =
          Plan.findAllWhere
            Foo.table
            Foo.fooNameField
            (Orville.fieldGreaterThan Foo.fooAgeField Foo.averageFooAge)

    (targetName, foos) <- HH.forAll generateSearchTargetAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetName

    let isMatch foo = Foo.hasName targetName foo && Foo.fooAge foo > Foo.averageFooAge

    coverSearchResultCases isMatch foos
    assertAllMatchesFound Foo.fooId results isMatch foos

prop_planMany_findAllWhere :: Property.NamedDBProperty
prop_planMany_findAllWhere =
  Property.namedDBProperty "(planMany findAllWhere) finds all rows where the field matches for each list of inputs, with the given condition" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName [Foo.Foo])
        plan =
          Plan.planMany $
            Plan.findAllWhere
              Foo.table
              Foo.fooNameField
              (Orville.fieldGreaterThan Foo.fooAgeField Foo.averageFooAge)

    (targetNames, foos) <- HH.forAll generateSearchTargetListAndSubjects
    results <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan targetNames

    let isMatch foo = elem (Foo.fooName foo) targetNames && Foo.fooAge foo > Foo.averageFooAge

    coverSearchResultCases isMatch foos
    assertEachManyResult targetNames results $ \targetName foundFoos ->
      assertAllMatchesFound Foo.fooId foundFoos (\foo -> Foo.hasName targetName foo && isMatch foo) foos

prop_planEither :: Property.NamedDBProperty
prop_planEither =
  Property.namedDBProperty "planEither executes either the right or left plan based on input" $ \pool -> do
    let plan :: Plan.Plan scope (Either Foo.FooId Foo.FooName) Foo.Foo
        plan =
          either id id
            <$> Plan.planEither
              (Plan.findOne Foo.table Foo.fooIdField)
              (Plan.findOne Foo.table Foo.fooNameField)

    foo <- HH.forAll Foo.generate
    param <-
      HH.forAll . Gen.element $
        [ Left (Foo.fooId foo)
        , Right (Foo.fooName foo)
        ]

    foundFoo <-
      Foo.withTable pool $ do
        _ <- Orville.insertEntity Foo.table foo
        Plan.execute plan param

    foundFoo === foo

prop_planMany_planEither :: Property.NamedDBProperty
prop_planMany_planEither =
  Property.namedDBProperty "(planMany planEither) executes either the right and left plans with appropriate inputs" $ \pool -> do
    let plan :: Plan.Plan scope [Either Foo.FooId Foo.FooName] (Many.Many (Either Foo.FooId Foo.FooName) Foo.Foo)
        plan =
          Plan.planMany $
            either id id
              <$> Plan.planEither
                (Plan.findOne Foo.table Foo.fooIdField)
                (Plan.findOne Foo.table Foo.fooNameField)

    foos <- HH.forAll $ Foo.generateList (Range.linear 0 10)

    let pickInput :: Foo.Foo -> HH.PropertyT IO (Either Foo.FooId Foo.FooName)
        pickInput foo =
          HH.forAll . Gen.element $
            [ Left (Foo.fooId foo)
            , Right (Foo.fooName foo)
            ]

    params <- traverse pickInput foos

    foundFoos <-
      Foo.withTable pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Plan.execute plan params

    assertEachManyResult params foundFoos $ \input foo ->
      case input of
        Left fooId ->
          Foo.fooId foo === fooId
        Right fooName ->
          Foo.fooName foo === fooName

prop_bindAndUse :: Property.NamedDBProperty
prop_bindAndUse =
  Property.namedDBProperty "bind/use allows caller to use plan output as input for another plan" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName (Foo.Foo, [FooChild.FooChild])
        plan =
          Plan.bind (Plan.findOne Foo.table Foo.fooNameField) $ \foo ->
            let fooId = Foo.fooId <$> foo
             in (,)
                  <$> Plan.use foo
                  <*> Plan.using fooId (Plan.findAll FooChild.table FooChild.fooChildFooIdField)

    foo <- HH.forAll Foo.generate
    fooChild <- HH.forAll $ FooChild.generate [foo]

    result <-
      FooChild.withTables pool $ do
        _ <- Orville.insertEntity Foo.table foo
        _ <- Orville.insertEntity FooChild.table fooChild
        Plan.execute plan (Foo.fooName foo)

    result === (foo, [fooChild])

prop_planMany_bindAndUse :: Property.NamedDBProperty
prop_planMany_bindAndUse =
  Property.namedDBProperty "(planMany bind/use) allows caller to use plan output as input for another plan" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName (Foo.Foo, [FooChild.FooChild]))
        plan =
          Plan.planMany $
            Plan.bind (Plan.findOne Foo.table Foo.fooNameField) $ \foo ->
              let fooId = Foo.fooId <$> foo
               in (,)
                    <$> Plan.use foo
                    <*> Plan.using fooId (Plan.findAll FooChild.table FooChild.fooChildFooIdField)

    allFoos <- HH.forAll $ Foo.generateList (Range.linear 1 5)
    allChildren <- HH.forAll $ FooChild.generateList (Range.linear 0 20) allFoos

    let allFooNames = Foo.fooName <$> allFoos

    results <-
      FooChild.withTables pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty allFoos)
        traverse_ (Orville.insertEntities FooChild.table) (NEL.nonEmpty allChildren)
        Plan.execute plan allFooNames

    assertEachManyResult allFooNames results $ \fooName (foo, children) -> do
      Foo.fooName foo === fooName
      assertAllMatchesFound FooChild.fooChildId children (FooChild.isChildOf foo) allChildren

prop_planMany_findOne_dedupesInClauses :: Property.NamedDBProperty
prop_planMany_findOne_dedupesInClauses =
  Property.singletonNamedDBProperty "planMany/findOne dedupes in clause to avoid PostgreSQL parameter limit" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooId] (Many.Many Foo.FooId Foo.Foo)
        plan =
          Plan.planMany (Plan.findOne Foo.table Foo.fooIdField)

    unsavedFoo <- HH.forAll Foo.generate

    results <-
      FooChild.withTables pool $ do
        savedFoo <- Orville.insertAndReturnEntity Foo.table unsavedFoo
        let fooIds =
              replicate 65536 (Foo.fooId savedFoo)

        Plan.execute plan fooIds

    length (Many.elems results) === 65536

#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)
prop_bindAndUse_qualifiedDo :: Property.NamedDBProperty
prop_bindAndUse_qualifiedDo =
  Property.namedDBProperty "bind/use allows caller to use plan output as input for another plan (QualifiedDo version)" $ \pool -> do
    let plan :: Plan.Plan scope Foo.FooName (Foo.Foo, [FooChild.FooChild])
        plan = PlanSyntax.do
          foo <- Plan.findOne Foo.table Foo.fooNameField

          let fooId = Foo.fooId <$> foo

          children <-
            Plan.using fooId $
              Plan.findAll FooChild.table FooChild.fooChildFooIdField

          (,)
            <$> Plan.use foo
            <*> Plan.use children

    foo <- HH.forAll Foo.generate
    fooChild <- HH.forAll $ FooChild.generate [foo]

    result <-
      FooChild.withTables pool $ do
        _ <- Orville.insertEntity Foo.table foo
        _ <- Orville.insertEntity FooChild.table fooChild
        Plan.execute plan (Foo.fooName foo)

    result === (foo, [fooChild])

prop_planMany_bindAndUse_qualifiedDo :: Property.NamedDBProperty
prop_planMany_bindAndUse_qualifiedDo =
  Property.namedDBProperty "(planMany bind/use) allows caller to use plan output as input for another plan (QualifiedDo version)" $ \pool -> do
    let plan :: Plan.Plan scope [Foo.FooName] (Many.Many Foo.FooName (Foo.Foo, [FooChild.FooChild]))
        plan =
          Plan.planMany $ PlanSyntax.do
            foo <- Plan.findOne Foo.table Foo.fooNameField

            let fooId = Foo.fooId <$> foo

            children <-
              Plan.using fooId $
                Plan.findAll FooChild.table FooChild.fooChildFooIdField

            (,)
              <$> Plan.use foo
              <*> Plan.use children

    allFoos <- HH.forAll $ Foo.generateList (Range.linear 1 5)
    allChildren <- HH.forAll $ FooChild.generateList (Range.linear 0 20) allFoos

    let allFooNames = Foo.fooName <$> allFoos

    results <-
      FooChild.withTables pool $ do
        traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty allFoos)
        traverse_ (Orville.insertEntities FooChild.table) (NEL.nonEmpty allChildren)
        Plan.execute plan allFooNames

    assertEachManyResult allFooNames results $ \fooName (foo, children) -> do
      Foo.fooName foo === fooName
      assertAllMatchesFound FooChild.fooChildId children (FooChild.isChildOf foo) allChildren
#endif
#endif

prop_assert :: Property.NamedDBProperty
prop_assert =
  Property.namedDBProperty "assert raises an AssertionError in IO" $ \pool -> do
    let plan :: Plan.Plan scope (Either String ()) ()
        plan =
          Plan.assert (\_ value -> value) Plan.askParam

    input <- HH.forAll $ Gen.element [Left "Test Error", Right ()]
    result <- MIO.liftIO $ Exception.try $ Orville.runOrville pool (Plan.execute plan input)
    Either.isLeft (result :: Either Plan.AssertionFailed ()) === Either.isLeft input

prop_explain :: Property.NamedProperty
prop_explain =
  Property.namedProperty "explain shows the SQL steps that will be executed" $ do
    let plan :: Plan.Plan scope Foo.FooName [FooChild.FooChild]
        plan =
          Plan.chain
            (Plan.findOne Foo.table Foo.fooNameField)
            (Plan.focusParam Foo.fooId $ Plan.findAll FooChild.table FooChild.fooChildFooIdField)

        explanation =
          Plan.explain plan

    explanation
      === [ "SELECT \"name\",\"id\",\"name\",\"age\" FROM \"foo\" WHERE (\"name\") = ($1)"
          , "SELECT \"foo_id\",\"id\",\"foo_id\" FROM \"foo_child\" WHERE (\"foo_id\") = ($1)"
          ]

{- |
  Generates a list of Foos that along with FooName that could plausibly be
  found in the list zero, one or more times.
-}
generateSearchTargetAndSubjects :: HH.Gen (Foo.FooName, [Foo.Foo])
generateSearchTargetAndSubjects = do
  targetName <- Foo.generateFooName

  let generatePossibleTargetFoo =
        Gen.choice
          [ Foo.generateFooWithName targetName
          , Foo.generate
          ]

  foos <- Foo.generateListUsing (Range.linear 0 5) generatePossibleTargetFoo
  pure (targetName, foos)

{- |
  Generates a list of Foos that along with FooName that could plausibly be
  found in the list zero, one or more times.
-}
generateSearchTargetListAndSubjects :: HH.Gen ([Foo.FooName], [Foo.Foo])
generateSearchTargetListAndSubjects = do
  targetNames <- Gen.list (Range.linear 0 5) Foo.generateFooName

  let generatePossibleTargetFoo =
        Gen.choice (Foo.generate : fmap Foo.generateFooWithName targetNames)

  foos <- Foo.generateListUsing (Range.linear 0 5) generatePossibleTargetFoo
  pure (targetNames, foos)

{- |
  Uses Hedgehog's cover function to make sure common edge cases are covered
  for tests that are conducting a search. The predicate given should indicate
  whether the item would be expected to match the search being tested. The
  list of items should be the list that will be searced against.
-}
coverSearchResultCases :: HH.MonadTest m => (a -> Bool) -> [a] -> m ()
coverSearchResultCases predicate subjects = do
  HH.cover 1 (String.fromString "no search subjects") (null subjects)
  HH.cover 1 (String.fromString "subjects present with no matches") (not (null subjects) && not (any predicate subjects))
  HH.cover 1 (String.fromString "at least 1 match") (length (filter predicate subjects) >= 1)

{- |
  Asserts that the given result is one that could have been produced by apply
  the predicate to the given list to find a single result. This assertion
  doesn't care which particular item from the list was found, as long as it the
  result matches the predicate and it is one from the list, or that nothing in
  the list matches if the 'Nothing' was found.
-}
assertMatchIsFromPredicate ::
  (HH.MonadTest m, Eq entity) =>
  Maybe entity ->
  (entity -> Bool) ->
  [entity] ->
  m ()
assertMatchIsFromPredicate maybeEntity predicate subjects =
  case maybeEntity of
    Nothing ->
      HH.assert (not $ any predicate subjects)
    Just entity ->
      HH.assert (predicate entity && elem entity subjects)

{- |
  Asserts that the found items are all of those from the list that match the
  predicate, but without caring about order.
-}
assertAllMatchesFound ::
  (HH.MonadTest m, Ord key, Eq entity, Show entity) =>
  -- | Key value to sort on to eliminate order dependencies
  (entity -> key) ->
  -- | the actual matches
  [entity] ->
  -- | the predicate to use for matching
  (entity -> Bool) ->
  -- | the full search space
  [entity] ->
  m ()
assertAllMatchesFound keyAttr foundEntities predicate allEntities =
  List.sortOn keyAttr foundEntities === List.sortOn keyAttr (filter predicate allEntities)

{- |
  Applies the given assertion for every key in the list. If any of the key
  is not found in the provided 'Many.Many' value, this assertion will fail.
-}
assertEachManyResult ::
  (Show key, HH.MonadTest m) =>
  [key] ->
  Many.Many key value ->
  (key -> value -> m ()) ->
  m ()
assertEachManyResult expectedKeys manyValues assertion = do
  let checkResult key =
        case Many.lookup key manyValues of
          Left Many.NotAKey -> do
            HH.footnote $ "Expected " <> show key <> " to be a key in results, but it was not"
            HH.failure
          Right value ->
            assertion key value

  traverse_ checkResult expectedKeys
