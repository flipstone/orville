{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Interactions to work with database sequence values on the Haskell side. Including inspection of the
current and next values in the sequence as well as updating a sequence to a given value.

@since 0.10.0.0
-}
module Orville.PostgreSQL.Execution.Sequence
  ( sequenceNextValue
  , sequenceCurrentValue
  , sequenceSetValue
  )
where

import Data.Int (Int64)

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RowCountExpectation as RowCountExpectation
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import Orville.PostgreSQL.Schema (SequenceDefinition, sequenceName)

{- |
  Fetches the next value from a sequence via the PostgreSQL @nextval@ function.

@since 0.10.0.0
-}
sequenceNextValue :: Monad.MonadOrville m => SequenceDefinition -> m Int64
sequenceNextValue sequenceDef =
  selectInt64Value
    "sequenceNextValue"
    (Expr.nextVal (sequenceName sequenceDef))

{- |
  Fetches the current value from a sequence via the PostgreSQL @currval@ function.

@since 0.10.0.0
-}
sequenceCurrentValue :: Monad.MonadOrville m => SequenceDefinition -> m Int64
sequenceCurrentValue sequenceDef =
  selectInt64Value
    "sequenceCurrentValue"
    (Expr.currVal (sequenceName sequenceDef))

{- |
  Sets the current value from a sequence via the PostgreSQL @setval@ function.

@since 0.10.0.0
-}
sequenceSetValue :: Monad.MonadOrville m => SequenceDefinition -> Int64 -> m Int64
sequenceSetValue sequenceDef newValue =
  selectInt64Value
    "sequenceSetValue"
    (Expr.setVal (sequenceName sequenceDef) newValue)

selectInt64Value :: Monad.MonadOrville m => String -> Expr.ValueExpression -> m Int64
selectInt64Value caller valueExpression = do
  let
    queryExpr =
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
