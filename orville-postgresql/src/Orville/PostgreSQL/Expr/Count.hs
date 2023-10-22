{- |
Copyright : Flipstone Technology Partners 2023
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

import Orville.PostgreSQL.Expr.Name (ColumnName, FunctionName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, columnReference, functionCall)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | The SQL @count@ function.

@since 1.0.0.0
-}
countFunction :: FunctionName
countFunction = functionName "count"

{- | Given a 'ValueExpression', use it as the argument to the SQL @count@.

@since 1.0.0.0
-}
count :: ValueExpression -> ValueExpression
count value =
  functionCall countFunction [value]

{- | The SQL @count(1)@.

@since 1.0.0.0
-}
count1 :: ValueExpression
count1 =
  count . RawSql.unsafeFromRawSql . RawSql.intDecLiteral $ 1

{- | Use a given column as the argument to the SQL @count@.

@since 1.0.0.0
-}
countColumn :: ColumnName -> ValueExpression
countColumn =
  count . columnReference
