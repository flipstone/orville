module Orville.PostgreSQL.Internal.Execute
  ( executeAndDecode,
    executeAndReturnAffectedRows,
    executeVoid,
    executeAndDecodeIO,
    executeAndReturnAffectedRowsIO,
    executeVoidIO,
    AffectedRowsDecodingError,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Connection (Connection)
import Orville.PostgreSQL.Internal.MonadOrville (MonadOrville, withConnection)
import Orville.PostgreSQL.Internal.OrvilleState (OrvilleState, askOrvilleState, orvilleErrorDetailLevel, orvilleSqlCommenter, orvilleSqlExecutionCallback)
import Orville.PostgreSQL.Internal.QueryType (QueryType)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlCommenter as SqlCommenter
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

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
  Executes a SQL query and returns the number of rows affected by the query.
  Any SQL Execution callbacks that have been added to the 'OrvilleState' will
  be called.

  This function can only be used for the execution of a SELECT, CREATE
  TABLE AS, INSERT, UPDATE, DELETE, MOVE, FETCH, or COPY statement, or an
  EXECUTE of a prepared query that contains an INSERT, UPDATE, or DELETE
  statement. If the query is anything else an 'AffectedRowsDecodingError'
  wil be raised after the query is executed when the result is read.

  If the query fails an exception will be raised.
-}
executeAndReturnAffectedRows ::
  (MonadOrville m, RawSql.SqlExpression sql) =>
  QueryType ->
  sql ->
  m Int
executeAndReturnAffectedRows queryType sql = do
  orvilleState <- askOrvilleState
  withConnection (liftIO . executeAndReturnAffectedRowsIO queryType sql orvilleState)

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
  Executes a SQL query and returns the number of rows affected by the query.
  Any SQL Execution callbacks that have been added to the 'OrvilleState' will
  be called.

  This function can only be used for the execution of a SELECT, CREATE
  TABLE AS, INSERT, UPDATE, DELETE, MOVE, FETCH, or COPY statement, or an
  EXECUTE of a prepared query that contains an INSERT, UPDATE, or DELETE
  statement. If the query is anything else an 'AffectedRowsDecodingError'
  wil be raised after the query is executed when the result is read.

  If the query fails an exception will be raised.
-}
executeAndReturnAffectedRowsIO ::
  RawSql.SqlExpression sql =>
  QueryType ->
  sql ->
  OrvilleState ->
  Connection ->
  IO Int
executeAndReturnAffectedRowsIO queryType sql orvilleState conn = do
  libPqResult <- executeWithCallbacksIO queryType sql orvilleState conn
  mbTupleCount <- LibPQ.cmdTuples libPqResult
  case mbTupleCount of
    Nothing ->
      throwIO
        . AffectedRowsDecodingError
        $ "No affected row count was produced by the query"
    Just bs ->
      case SqlValue.toInt (SqlValue.fromRawBytes bs) of
        Left err ->
          throwIO . AffectedRowsDecodingError $ err
        Right n ->
          pure n

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
  let rawSql =
        case orvilleSqlCommenter orvilleState of
          Nothing ->
            RawSql.toRawSql sql
          Just sqlCommenter ->
            SqlCommenter.addComment sqlCommenter $ RawSql.toRawSql sql
   in orvilleSqlExecutionCallback
        orvilleState
        queryType
        rawSql
        (RawSql.execute conn rawSql)

{- |
  Thrown by 'executeAndReturnAffectedRows' and 'executeAndReturnAffectedRowsIO'
  if the number of affected rows cannot be successfully read from the LibPQ
  command result.
-}
newtype AffectedRowsDecodingError
  = AffectedRowsDecodingError String
  deriving (Show)

instance Exception AffectedRowsDecodingError
