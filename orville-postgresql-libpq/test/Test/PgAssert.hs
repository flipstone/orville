module Test.PgAssert
  ( assertTableExists,
    assertColumnNamesEqual,
    assertColumnExists,
    assertUniqueConstraintExists,
    assertForeignKeyConstraintExists,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.String as String
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

assertTableExists ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  m PgCatalog.RelationDescription
assertTableExists pool tableName = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabaseRelations
          [(String.fromString "public", String.fromString tableName)]

  case PgCatalog.lookupRelation (String.fromString "public", String.fromString tableName) dbDesc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ tableName <> " table not found"
        HH.failure
    Just rel ->
      pure rel

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

assertForeignKeyConstraintExists ::
  (HH.MonadTest m, HasCallStack) =>
  PgCatalog.RelationDescription ->
  NEL.NonEmpty (String, String) ->
  m ()
assertForeignKeyConstraintExists relationDesc foreignReferences = do
  let attNames = map (String.fromString . fst) (NEL.toList foreignReferences)
      foreignNames = map (String.fromString . snd) (NEL.toList foreignReferences)
      constraintMatches constraintDesc =
        and
          [ PgCatalog.pgConstraintType (PgCatalog.constraintRecord constraintDesc) == PgCatalog.ForeignKeyConstraint
          , fmap (map PgCatalog.pgAttributeName) (PgCatalog.constraintKey constraintDesc) == Just attNames
          , fmap (map PgCatalog.pgAttributeName) (PgCatalog.constraintForeignKey constraintDesc) == Just foreignNames
          ]

  Monad.when (not $ isMatchingConstraintPresent constraintMatches relationDesc) $
    withFrozenCallStack $ do
      HH.annotate $ "Foreign key constraint " <> show (NEL.toList foreignReferences) <> " not found"
      HH.failure

isMatchingConstraintPresent ::
  (PgCatalog.ConstraintDescription -> Bool) ->
  PgCatalog.RelationDescription ->
  Bool
isMatchingConstraintPresent predicate relationDesc =
  List.any
    predicate
    (PgCatalog.relationConstraints relationDesc)
