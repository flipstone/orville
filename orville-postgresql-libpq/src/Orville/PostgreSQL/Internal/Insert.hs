{-# LANGUAGE GADTs #-}

{- |

Module    : Orville.PostgreSQL.Internal.Insert
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Insert (Insert, insertToInsertExpr, executeInsert, executeInsertReturnEntities, insertToTableReturning, insertToTable) where

import Data.List.NonEmpty (NonEmpty)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller)
import Orville.PostgreSQL.Internal.TableDefinition (ReturningOption (WithReturning, WithoutReturning), TableDefinition, mkInsertExpr, tableMarshaller)

data Insert readEntity where
  Insert :: SqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity

insertToInsertExpr :: Insert readEntity -> Expr.InsertExpr
insertToInsertExpr (Insert _ expr) = expr

executeInsert :: MonadOrville.MonadOrville m => Insert readEntity -> m ()
executeInsert (Insert _ expr) =
  Execute.executeVoid expr

executeInsertReturnEntities :: MonadOrville.MonadOrville m => Insert readEntity -> m [readEntity]
executeInsertReturnEntities (Insert marshaller expr) =
  Execute.executeAndDecode expr marshaller

insertToTable ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertToTable =
  insertTable WithoutReturning

insertToTableReturning ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertToTableReturning =
  insertTable WithReturning

insertTable ::
  ReturningOption ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Insert readEntity
insertTable returningOption tableDef entities =
  insertExpr (tableMarshaller tableDef) (mkInsertExpr returningOption tableDef entities)

insertExpr :: SqlMarshaller writeEntity readEntity -> Expr.InsertExpr -> Insert readEntity
insertExpr = Insert
