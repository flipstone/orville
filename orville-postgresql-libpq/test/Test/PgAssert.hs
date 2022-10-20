module Test.PgAssert
  ( assertTableExists,
    assertTableDoesNotExist,
    assertTableExistsInSchema,
    assertColumnNamesEqual,
    assertColumnExists,
    assertColumnDefaultExists,
    assertColumnDefaultMatches,
    assertUniqueConstraintExists,
    assertForeignKeyConstraintExists,
    assertIndexExists,
    ForeignKeyInfo (..),
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text.Encoding as Enc
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

assertTableExists ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  m PgCatalog.RelationDescription
assertTableExists pool =
  assertTableExistsInSchema pool "public"

assertTableDoesNotExist ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  m ()
assertTableDoesNotExist pool =
  assertTableDoesNotExistInSchema pool "public"

assertTableExistsInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  String ->
  m PgCatalog.RelationDescription
assertTableExistsInSchema pool schemaName tableName = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabaseRelations
          [(String.fromString schemaName, String.fromString tableName)]

  case PgCatalog.lookupRelation (String.fromString schemaName, String.fromString tableName) dbDesc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ tableName <> " table not found"
        HH.failure
    Just rel ->
      pure rel

assertTableDoesNotExistInSchema ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  String ->
  m ()
assertTableDoesNotExistInSchema pool schemaName tableName = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabaseRelations
          [(String.fromString schemaName, String.fromString tableName)]

  case PgCatalog.lookupRelation (String.fromString schemaName, String.fromString tableName) dbDesc of
    Nothing ->
      pure ()
    Just _ ->
      withFrozenCallStack $ do
        HH.annotate $ tableName <> " table expected to not be present, but was found"
        HH.failure

assertColumnNamesEqual ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  [String] ->
  m ()
assertColumnNamesEqual relationDesc expectedColumns = do
  let attributeNames =
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

  let actualDefault = PgCatalog.lookupAttributeDefault attr relationDesc

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

  let actualDefault = PgCatalog.lookupAttributeDefault attr relationDesc

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
        let expectedBytes =
              RawSql.toExampleBytes
                . Orville.defaultValueExpression
                $ defaultValue

            actualBytes =
              Enc.encodeUtf8
                . PgCatalog.pgAttributeDefaultExpression
                $ pgDefault
         in expectedBytes === actualBytes

assertUniqueConstraintExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  NEL.NonEmpty String ->
  m ()
assertUniqueConstraintExists relationDesc columnNames = do
  let attNames = map String.fromString (NEL.toList columnNames)
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
  let attNames = map (String.fromString . fst) (NEL.toList $ foreignKeyInfoReferences foreignKeyInfo)
      foreignNames = map (String.fromString . snd) (NEL.toList $ foreignKeyInfoReferences foreignKeyInfo)

      constraintMatches constraintDesc =
        let constraintRec = PgCatalog.constraintRecord constraintDesc
         in and
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
  let expectedMemberNames =
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
