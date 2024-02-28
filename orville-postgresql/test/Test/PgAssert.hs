module Test.PgAssert
  ( assertTableExists
  , assertTableExistsInSchema
  , assertTableDoesNotExist
  , assertTableDoesNotExistInSchema
  , assertSequenceExists
  , assertSequenceExistsInSchema
  , assertSequenceDoesNotExist
  , assertSequenceDoesNotExistInSchema
  , assertRelationHasPgSequence
  , assertColumnNamesEqual
  , assertColumnExists
  , assertColumnDefaultExists
  , assertColumnDefaultMatches
  , assertUniqueConstraintExists
  , assertForeignKeyConstraintExists
  , assertIndexExists
  , assertFunctionExists
  , assertFunctionExistsInSchema
  , assertFunctionDoesNotExist
  , assertFunctionDoesNotExistInSchema
  , assertTriggerExists
  , assertTriggerDoesNotExist
  , ForeignKeyInfo (..)
  )
where

import qualified Control.Exception.Safe as SafeEx
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.String as String
import qualified Data.Text.Encoding as Enc
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

assertTableExists ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m PgCatalog.RelationDescription
assertTableExists pool =
  assertTableExistsInSchema pool "public"

assertTableDoesNotExist ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m ()
assertTableDoesNotExist pool =
  assertTableDoesNotExistInSchema pool "public"

assertTableExistsInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m PgCatalog.RelationDescription
assertTableExistsInSchema pool schemaName tableName =
  assertRelationExistsInSchema pool schemaName tableName PgCatalog.OrdinaryTable

assertTableDoesNotExistInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m ()
assertTableDoesNotExistInSchema pool schemaName tableName =
  assertRelationDoesNotExistInSchema pool schemaName tableName PgCatalog.OrdinaryTable

assertSequenceExists ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m PgCatalog.RelationDescription
assertSequenceExists pool =
  assertSequenceExistsInSchema pool "public"

assertSequenceDoesNotExist ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m ()
assertSequenceDoesNotExist pool =
  assertSequenceDoesNotExistInSchema pool "public"

assertSequenceExistsInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m PgCatalog.RelationDescription
assertSequenceExistsInSchema pool schemaName sequenceName =
  assertRelationExistsInSchema pool schemaName sequenceName PgCatalog.Sequence

assertSequenceDoesNotExistInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m ()
assertSequenceDoesNotExistInSchema pool schemaName sequenceName =
  assertRelationDoesNotExistInSchema pool schemaName sequenceName PgCatalog.Sequence

assertRelationHasPgSequence ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  m PgCatalog.PgSequence
assertRelationHasPgSequence relationDesc =
  case PgCatalog.relationSequence relationDesc of
    Nothing ->
      withFrozenCallStack $ do
        HH.annotate $ "Expected relation to have a PgSequence value, but it did not"
        HH.failure
    Just pgSequence ->
      pure pgSequence

assertRelationExistsInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  PgCatalog.RelationKind ->
  m PgCatalog.RelationDescription
assertRelationExistsInSchema pool schemaName relationName relationKind = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabase
          [(String.fromString schemaName, String.fromString relationName)]
          []

  case PgCatalog.lookupRelation (String.fromString schemaName, String.fromString relationName) dbDesc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ relationName <> " relation not found"
        HH.failure
    Just rel -> do
      PgCatalog.pgClassRelationKind (PgCatalog.relationRecord rel) === relationKind
      pure rel

assertRelationDoesNotExistInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  PgCatalog.RelationKind ->
  m ()
assertRelationDoesNotExistInSchema pool schemaName tableName relationKind = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $
        PgCatalog.describeDatabase
          [(String.fromString schemaName, String.fromString tableName)]
          []

  case PgCatalog.lookupRelation (String.fromString schemaName, String.fromString tableName) dbDesc of
    Nothing ->
      pure ()
    Just rel ->
      if PgCatalog.pgClassRelationKind (PgCatalog.relationRecord rel) == relationKind
        then withFrozenCallStack $ do
          HH.annotate $
            tableName
              <> " relation expected to not be present with kind "
              <> show relationKind
              <> ", but it was found"

          HH.failure
        else pure ()

assertColumnNamesEqual ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  [String] ->
  m ()
assertColumnNamesEqual relationDesc expectedColumns = do
  let
    attributeNames =
      fmap PgCatalog.pgAttributeName
        . filter PgCatalog.isOrdinaryColumn
        . Map.elems
        . PgCatalog.relationAttributes
        $ relationDesc

  withFrozenCallStack $
    List.sort attributeNames === List.sort (map String.fromString expectedColumns)

assertColumnExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  String ->
  m PgCatalog.PgAttribute
assertColumnExists relationDesc columnName = do
  case PgCatalog.lookupAttribute (String.fromString columnName) relationDesc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ columnName <> " column not found"
        HH.failure
    Just attr ->
      pure attr

assertColumnDefaultExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  String ->
  m ()
assertColumnDefaultExists relationDesc columnName = do
  attr <- assertColumnExists relationDesc columnName

  let
    actualDefault = PgCatalog.lookupAttributeDefault attr relationDesc

  withFrozenCallStack $
    case actualDefault of
      Nothing -> do
        HH.annotate $ columnName <> " expected to have a default, but it did not"
        HH.failure
      Just _ ->
        pure ()

assertColumnDefaultMatches ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  String ->
  Maybe (Orville.DefaultValue a) ->
  m ()
assertColumnDefaultMatches relationDesc columnName expectedDefault = do
  attr <- assertColumnExists relationDesc columnName

  let
    actualDefault = PgCatalog.lookupAttributeDefault attr relationDesc

  withFrozenCallStack $
    case (expectedDefault, actualDefault) of
      (Nothing, Nothing) ->
        pure ()
      (Nothing, Just _) -> do
        HH.annotate $ columnName <> " was not expected to have a default, but it did"
        HH.failure
      (Just _, Nothing) -> do
        HH.annotate $ columnName <> " expected to have a default, but it did not"
        HH.failure
      (Just defaultValue, Just pgDefault) ->
        let
          expectedBytes =
            RawSql.toExampleBytes
              . Orville.defaultValueExpression
              $ defaultValue

          actualBytes =
            Enc.encodeUtf8
              . PgCatalog.pgAttributeDefaultExpression
              $ pgDefault
        in
          expectedBytes === actualBytes

assertUniqueConstraintExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  NEL.NonEmpty String ->
  m ()
assertUniqueConstraintExists relationDesc columnNames = do
  let
    attNames = map String.fromString (NEL.toList columnNames)
    constraintMatches constraintDesc =
      and
        [ PgCatalog.pgConstraintType (PgCatalog.constraintRecord constraintDesc) == PgCatalog.UniqueConstraint
        , fmap (map PgCatalog.pgAttributeName) (PgCatalog.constraintKey constraintDesc) == Just attNames
        ]

  Monad.when (not $ isMatchingConstraintPresent constraintMatches relationDesc) $
    withFrozenCallStack $ do
      HH.annotate $ "Unique constraint " <> show (NEL.toList columnNames) <> " not found"
      HH.failure

data ForeignKeyInfo = ForeignKeyInfo
  { foreignKeyInfoReferences :: NEL.NonEmpty (String, String)
  , foreignKeyInfoOnUpdate :: Orville.ForeignKeyAction
  , foreignKeyInfoOnDelete :: Orville.ForeignKeyAction
  }
  deriving (Show, Eq)

assertForeignKeyConstraintExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  ForeignKeyInfo ->
  m ()
assertForeignKeyConstraintExists relationDesc foreignKeyInfo = do
  let
    attNames = map (String.fromString . fst) (NEL.toList $ foreignKeyInfoReferences foreignKeyInfo)
    foreignNames = map (String.fromString . snd) (NEL.toList $ foreignKeyInfoReferences foreignKeyInfo)

    constraintMatches constraintDesc =
      let
        constraintRec = PgCatalog.constraintRecord constraintDesc
      in
        and
          [ PgCatalog.pgConstraintType constraintRec == PgCatalog.ForeignKeyConstraint
          , fmap (map PgCatalog.pgAttributeName) (PgCatalog.constraintKey constraintDesc) == Just attNames
          , fmap (map PgCatalog.pgAttributeName) (PgCatalog.constraintForeignKey constraintDesc) == Just foreignNames
          , PgCatalog.pgConstraintForeignKeyOnUpdateType constraintRec == Just (foreignKeyInfoOnUpdate foreignKeyInfo)
          , PgCatalog.pgConstraintForeignKeyOnDeleteType constraintRec == Just (foreignKeyInfoOnDelete foreignKeyInfo)
          ]

  Monad.when (not $ isMatchingConstraintPresent constraintMatches relationDesc) $
    withFrozenCallStack $ do
      HH.annotate $ "Foreign key constraint " <> show (NEL.toList $ foreignKeyInfoReferences foreignKeyInfo) <> " not found"
      HH.failure

isMatchingConstraintPresent ::
  (PgCatalog.ConstraintDescription -> Bool) ->
  PgCatalog.RelationDescription ->
  Bool
isMatchingConstraintPresent predicate relationDesc =
  List.any
    predicate
    (PgCatalog.relationConstraints relationDesc)

assertIndexExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  Orville.IndexUniqueness ->
  NEL.NonEmpty String ->
  m ()
assertIndexExists relationDesc uniqueness columnNames = do
  let
    expectedMemberNames =
      map (Just . String.fromString) (NEL.toList columnNames)

    memberAttributeName member =
      case member of
        PgCatalog.IndexAttribute attr -> Just $ PgCatalog.pgAttributeName attr
        PgCatalog.IndexExpression -> Nothing

    isUnique =
      case uniqueness of
        Orville.UniqueIndex -> True
        Orville.NonUniqueIndex -> False

    shouldIgnoreConstraintIndex constraint =
      elem
        (PgCatalog.pgConstraintType constraint)
        [ PgCatalog.PrimaryKeyConstraint
        , PgCatalog.UniqueConstraint
        , PgCatalog.ExclusionConstraint
        ]

    constraintIndexesToIgnore =
      map PgCatalog.pgConstraintIndexOid
        . filter shouldIgnoreConstraintIndex
        . map PgCatalog.constraintRecord
        . PgCatalog.relationConstraints
        $ relationDesc

    indexMatches indexDesc =
      and
        [ PgCatalog.pgIndexIsUnique (PgCatalog.indexRecord indexDesc) == isUnique
        , fmap memberAttributeName (PgCatalog.indexMembers indexDesc) == expectedMemberNames
        , not $ (elem (PgCatalog.pgIndexPgClassOid $ PgCatalog.indexRecord indexDesc) constraintIndexesToIgnore)
        ]

  Monad.when (not $ isMatchingIndexPresent indexMatches relationDesc) $
    withFrozenCallStack $ do
      HH.annotate $ show uniqueness <> " " <> show (NEL.toList columnNames) <> " not found"
      HH.failure

isMatchingIndexPresent ::
  (PgCatalog.IndexDescription -> Bool) ->
  PgCatalog.RelationDescription ->
  Bool
isMatchingIndexPresent predicate relationDesc =
  List.any
    predicate
    (PgCatalog.relationIndexes relationDesc)

assertFunctionExists ::
  (HH.MonadTest m, MIO.MonadIO m, SafeEx.MonadCatch m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m PgCatalog.PgProc
assertFunctionExists pool =
  assertFunctionExistsInSchema pool "public"

assertFunctionExistsInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, SafeEx.MonadCatch m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m PgCatalog.PgProc
assertFunctionExistsInSchema pool schemaName functionName = do
  namespaceOid <-
    HH.evalMaybeM
      . MIO.liftIO
      . Orville.runOrville pool
      . fmap (fmap PgCatalog.pgNamespaceOid)
      $ Orville.findFirstEntityBy
        PgCatalog.pgNamespaceTable
        ( Orville.where_
            (Orville.fieldEquals PgCatalog.namespaceNameField (String.fromString schemaName))
        )

  mbProc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.findFirstEntityBy
          PgCatalog.pgProcTable
          ( Orville.where_
              ( Orville.andExpr
                  (Orville.fieldEquals PgCatalog.procNamespaceOidField namespaceOid)
                  (Orville.fieldEquals PgCatalog.procNameField (String.fromString functionName))
              )
          )

  case mbProc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ functionName <> " function not found"
        HH.failure
    Just proc -> do
      pure proc

assertFunctionDoesNotExist ::
  (HH.MonadTest m, MIO.MonadIO m, SafeEx.MonadCatch m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  m ()
assertFunctionDoesNotExist pool =
  assertFunctionDoesNotExistInSchema pool "public"

assertFunctionDoesNotExistInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, SafeEx.MonadCatch m, HasCallStack) =>
  Orville.ConnectionPool ->
  String ->
  String ->
  m ()
assertFunctionDoesNotExistInSchema pool schemaName functionName = do
  namespaceOid <-
    HH.evalMaybeM
      . MIO.liftIO
      . Orville.runOrville pool
      . fmap (fmap PgCatalog.pgNamespaceOid)
      $ Orville.findFirstEntityBy
        PgCatalog.pgNamespaceTable
        ( Orville.where_
            (Orville.fieldEquals PgCatalog.namespaceNameField (String.fromString schemaName))
        )

  mbProc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.findFirstEntityBy
          PgCatalog.pgProcTable
          ( Orville.where_
              ( Orville.andExpr
                  (Orville.fieldEquals PgCatalog.procNamespaceOidField namespaceOid)
                  (Orville.fieldEquals PgCatalog.procNameField (String.fromString functionName))
              )
          )

  case mbProc of
    Nothing -> do
      pure ()
    Just _proc -> do
      withFrozenCallStack $ do
        HH.annotate $ functionName <> " function was found, but was not expected"
        HH.failure

assertTriggerExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  String ->
  m ()
assertTriggerExists relationDesc triggerName =
  let
    nameMatches trigger =
      PgCatalog.pgTriggerName trigger == String.fromString triggerName
  in
    case List.find nameMatches (PgCatalog.relationTriggers relationDesc) of
      Nothing ->
        withFrozenCallStack $ do
          HH.annotate $ triggerName <> " trigger not found"
          HH.failure
      Just _trigger ->
        pure ()

assertTriggerDoesNotExist ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  String ->
  m ()
assertTriggerDoesNotExist relationDesc triggerName =
  let
    nameMatches trigger =
      PgCatalog.pgTriggerName trigger == String.fromString triggerName
  in
    case List.find nameMatches (PgCatalog.relationTriggers relationDesc) of
      Nothing ->
        pure ()
      Just _trigger ->
        withFrozenCallStack $ do
          HH.annotate $ triggerName <> " trigger was found, but was note xpected"
          HH.failure
