{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Count
  ( count
  , countFunction
  , count1
  , countColumn
  )
where

import Orville.PostgreSQL.Expr.Name (ColumnName, FunctionName, QualifiedOrUnqualified, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, columnReference, functionCall)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{-# DEPRECATED countFunction "Use Orville.PostgreSQL.Expr.countFunctionName" #-}

{- | The SQL @count@ function.

@since 1.0.0.0
-}
countFunction :: FunctionName
countFunction = functionName "count"

{-# DEPRECATED count "Use Orville.PostgreSQL.Expr.countExprAggregateFunction" #-}

{- | Given a 'ValueExpression', use it as the argument to the SQL @count@.

@since 1.0.0.0
-}
count :: ValueExpression -> ValueExpression
count value =
  functionCall countFunction [value]

{-# DEPRECATED count1 "Use Orville.PostgreSQL.Expr.count1AggregateFunction" #-}

{- | The SQL @count(1)@.

@since 1.0.0.0
-}
count1 :: ValueExpression
count1 =
  count . RawSql.unsafeFromRawSql . RawSql.intDecLiteral $ 1

{-# DEPRECATED countColumn "Use Orville.PostgreSQL.Expr.countColumnAggregateFunction" #-}

{- | Use a given column as the argument to the SQL @count@.

@since 1.0.0.0
-}
countColumn :: QualifiedOrUnqualified ColumnName -> ValueExpression
countColumn =
  count . columnReference
