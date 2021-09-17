module Test.PgAssert
  ( assertColumnNamesEqual,
    assertColumnExists,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Pool as Pool
import qualified Data.String as String
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

assertColumnNamesEqual ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  [String] ->
  m ()
assertColumnNamesEqual pool tableName expectedColumns = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabaseRelations
          [(String.fromString "public", String.fromString tableName)]

  let attributeNames =
        fmap PgCatalog.pgAttributeName
          . filter PgCatalog.isOrdinaryColumn
          . concatMap (Map.elems . PgCatalog.relationAttributes)
          . Map.elems
          $ PgCatalog.databaseRelations dbDesc

  withFrozenCallStack $
    sort attributeNames === sort (map String.fromString expectedColumns)

assertColumnExists ::
  (HH.MonadTest m, MIO.MonadIO m, HasCallStack) =>
  Pool.Pool Conn.Connection ->
  String ->
  String ->
  m PgCatalog.PgAttribute
assertColumnExists pool tableName columnName = do
  dbDesc <-
    MIO.liftIO $
      Orville.runOrville pool $ do
        PgCatalog.describeDatabaseRelations
          [(String.fromString "public", String.fromString tableName)]

  relationDesc <-
    case PgCatalog.lookupRelation (String.fromString "public", String.fromString tableName) dbDesc of
      Nothing -> do
        withFrozenCallStack $ do
          HH.annotate $ tableName <> " table not found"
          HH.failure
      Just rel ->
        pure rel

  case PgCatalog.lookupAttribute (String.fromString columnName) relationDesc of
    Nothing -> do
      withFrozenCallStack $ do
        HH.annotate $ columnName <> " column not found"
        HH.failure
    Just attr ->
      pure attr
