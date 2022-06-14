module Orville.PostgreSQL.Internal.Execute
  ( executeAndDecode,
    executeVoid,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)

import Orville.PostgreSQL.Connection (Connection)
import Orville.PostgreSQL.Internal.MonadOrville (MonadOrville, withConnection)
import Orville.PostgreSQL.Internal.OrvilleState (askOrvilleState, orvilleErrorDetailLevel, orvilleSqlExecutionCallback)
import Orville.PostgreSQL.Internal.QueryType (QueryType)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller

{- |
  Executes a SQL query and decodes the result set using the provided
  marshaller. Any SQL Execution callbacks that have been added to the
  'OrvilleState' will be called.

  If the query fails or if any row is unable to be decoded by the marshaller,
  an exception will be raised.
-}
executeAndDecode ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  QueryType ->
  sql ->
  SqlMarshaller.AnnotatedSqlMarshaller writeEntity readEntity ->
  m [readEntity]
executeAndDecode queryType sql marshaller = do
  libPqResult <-
    executeWithCallbackUsing
      RawSql.execute
      queryType
      sql

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

{- |
  Executes a SQL query and ignores the result. Any SQL Execution callbacks
  that have been added to the 'OrvilleState' will be called.

  If the query fails an exception will be raised.
-}
executeVoid ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  QueryType ->
  sql ->
  m ()
executeVoid =
  executeWithCallbackUsing RawSql.executeVoid

executeWithCallbackUsing ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  (Connection -> RawSql.RawSql -> IO a) ->
  QueryType ->
  sql ->
  m a
executeWithCallbackUsing executeRawSql queryType sql = do
  orvilleState <- askOrvilleState

  let rawSql = RawSql.toRawSql sql

  withConnection $ \conn ->
    liftIO $
      orvilleSqlExecutionCallback
        orvilleState
        queryType
        rawSql
        (executeRawSql conn rawSql)
