module Orville.PostgreSQL.Internal.Execute
  ( executeAndDecode,
    executeVoid,
    executeAndDecodeIO,
    executeVoidIO,
  )
where

import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Connection (Connection)
import Orville.PostgreSQL.Internal.MonadOrville (MonadOrville, withConnection)
import Orville.PostgreSQL.Internal.OrvilleState (OrvilleState, askOrvilleState, orvilleErrorDetailLevel, orvilleSqlExecutionCallback)
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
  orvilleState <- askOrvilleState
  withConnection (liftIO . executeAndDecodeIO queryType sql marshaller orvilleState)

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
executeVoid queryType sql = do
  orvilleState <- askOrvilleState
  withConnection (liftIO . executeVoidIO queryType sql orvilleState)

{- |
  Executes a SQL query and decodes the result set using the provided
  marshaller. Any SQL Execution callbacks that have been added to the
  'OrvilleState' will be called.

  If the query fails or if any row is unable to be decoded by the marshaller,
  an exception will be raised.
-}
executeAndDecodeIO ::
  RawSql.SqlExpression sql =>
  QueryType ->
  sql ->
  SqlMarshaller.AnnotatedSqlMarshaller writeEntity readEntity ->
  OrvilleState ->
  Connection ->
  IO [readEntity]
executeAndDecodeIO queryType sql marshaller orvilleState conn = do
  libPqResult <- executeWithCallbacksIO queryType sql orvilleState conn

  let errorDetailLevel = orvilleErrorDetailLevel orvilleState

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
executeVoidIO ::
  RawSql.SqlExpression sql =>
  QueryType ->
  sql ->
  OrvilleState ->
  Connection ->
  IO ()
executeVoidIO queryType sql orvilleState =
  void . executeWithCallbacksIO queryType sql orvilleState

executeWithCallbacksIO ::
  RawSql.SqlExpression sql =>
  QueryType ->
  sql ->
  OrvilleState ->
  Connection ->
  IO LibPQ.Result
executeWithCallbacksIO queryType sql orvilleState conn =
  let rawSql = RawSql.toRawSql sql
   in orvilleSqlExecutionCallback
        orvilleState
        queryType
        rawSql
        (RawSql.execute conn rawSql)
