module Database.Orville.PostgreSQL.Internal.RecordOperations
  ( insertRecord,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.TableDefinition (TableDefinition, mkInsertExpr)

{- |
  Inserts a record into the specified table.

  TODO: This should return the 'readEntity' type using using the psql RETURNING
  clause and decoding the result set.
-}
insertRecord ::
  MonadOrville.MonadOrville m =>
  TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m ()
insertRecord entityTable entity = do
  let insertEntity = mkInsertExpr entityTable (entity :| [])
  MonadOrville.withConnection $ \connection ->
    liftIO $
      RawSql.executeVoid connection (Expr.insertExprToSql insertEntity)
