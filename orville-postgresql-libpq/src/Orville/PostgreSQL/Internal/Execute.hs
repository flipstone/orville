module Orville.PostgreSQL.Internal.Execute
  ( executeAndDecode,
    executeVoid,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)

import Orville.PostgreSQL.Internal.MonadOrville (MonadOrville, withConnection)
import Orville.PostgreSQL.Internal.OrvilleState (askOrvilleState, orvilleErrorDetailLevel)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller

executeAndDecode ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  sql ->
  SqlMarshaller.AnnotatedSqlMarshaller writeEntity readEntity ->
  m [readEntity]
executeAndDecode sql marshaller = do
  libPqResult <- withConnection (\conn -> liftIO $ RawSql.execute conn sql)

  errorDetailLevel <- fmap orvilleErrorDetailLevel askOrvilleState

  liftIO $ do
    decodingResult <-
      SqlMarshaller.marshallResultFromSql
        errorDetailLevel
        marshaller
        libPqResult

    case decodingResult of
      Left err ->
        throwIO err
      Right entities ->
        pure entities

executeVoid ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  sql ->
  m ()
executeVoid sql = do
  withConnection (\conn -> liftIO $ RawSql.executeVoid conn sql)
