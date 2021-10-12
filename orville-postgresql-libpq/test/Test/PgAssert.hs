module Test.PgAssert
  ( assertTableExists,
    assertColumnNamesEqual,
    assertColumnExists,
    assertUniqueConstraintExists,
  )
where

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
  attributes <- traverse (assertColumnExists relationDesc) columnNames

  let attNums = map PgCatalog.pgAttributeNumber . NEL.toList $ attributes
      constraintMatches constraint =
        PgCatalog.pgConstraintType constraint == PgCatalog.UniqueConstraint
          && PgCatalog.pgConstraintKey constraint == Just attNums

  case List.find constraintMatches (PgCatalog.relationConstraints relationDesc) of
    Nothing ->
      withFrozenCallStack $ do
        HH.annotate $ "Unique constraint " <> show (NEL.toList columnNames) <> " not found"
        HH.failure
    Just _ ->
      pure ()
