module Orville.PostgreSQL.Internal.Sequence
  ( sequenceNextValue,
    sequenceCurrentValue,
    sequenceSetValue,
  )
where

import Data.Int (Int64)

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.QueryType as QueryType
import qualified Orville.PostgreSQL.Internal.RowCountExpectation as RowCountExpectation
import Orville.PostgreSQL.Internal.SequenceDefinition (SequenceDefinition, sequenceName)
import qualified Orville.PostgreSQL.Marshall as Marshall

{- |
  Fetches the next value from a sequence via the PostgreSQL @nextval@ function.
-}
sequenceNextValue :: MonadOrville.MonadOrville m => SequenceDefinition -> m Int64
sequenceNextValue sequenceDef =
  selectInt64Value
    "sequenceNextValue"
    (Expr.nextVal (sequenceName sequenceDef))

{- |
  Fetches the current value from a sequence via the PostgreSQL @currval@ function.
-}
sequenceCurrentValue :: MonadOrville.MonadOrville m => SequenceDefinition -> m Int64
sequenceCurrentValue sequenceDef =
  selectInt64Value
    "sequenceCurrentValue"
    (Expr.currVal (sequenceName sequenceDef))

{- |
  Sets the current value from a sequence via the PostgreSQL @setval@ function.
-}
sequenceSetValue :: MonadOrville.MonadOrville m => SequenceDefinition -> Int64 -> m Int64
sequenceSetValue sequenceDef newValue =
  selectInt64Value
    "sequenceSetValue"
    (Expr.setVal (sequenceName sequenceDef) newValue)

selectInt64Value :: MonadOrville.MonadOrville m => String -> Expr.ValueExpression -> m Int64
selectInt64Value caller valueExpression = do
  let queryExpr =
        Expr.queryExpr
          (Expr.selectClause (Expr.selectExpr Nothing))
          ( Expr.selectDerivedColumns
              [Expr.deriveColumnAs valueExpression (Expr.columnName "result")]
          )
          Nothing

      marshaller =
        Marshall.annotateSqlMarshallerEmptyAnnotation
          . Marshall.marshallField id
          $ Marshall.bigIntegerField "result"
  values <- Execute.executeAndDecode QueryType.SelectQuery queryExpr marshaller
  RowCountExpectation.expectExactlyOneRow caller values
