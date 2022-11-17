module Orville.PostgreSQL.Internal.Expr.Count
  ( count,
    countFunction,
    count1,
    countColumn,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, FunctionName, functionName)
import Orville.PostgreSQL.Internal.Expr.ValueExpression (ValueExpression, columnReference, functionCall)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

countFunction :: FunctionName
countFunction = functionName "count"

count :: ValueExpression -> ValueExpression
count value =
  functionCall countFunction [value]

count1 :: ValueExpression
count1 =
  count . RawSql.unsafeFromRawSql . RawSql.intDecLiteral $ 1

countColumn :: ColumnName -> ValueExpression
countColumn =
  count . columnReference
