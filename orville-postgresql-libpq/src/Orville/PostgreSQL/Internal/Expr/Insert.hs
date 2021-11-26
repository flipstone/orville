{- |
Module    : Orville.PostgreSQL.Internal.Expr.Insert
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Insert
  ( InsertExpr,
    insertExpr,
    InsertColumnList,
    insertColumnList,
    InsertSource,
    insertSqlValues,
  )
where

import Orville.PostgreSQL.Internal.Expr.Insert.InsertColumnList (InsertColumnList, insertColumnList)
import Orville.PostgreSQL.Internal.Expr.Insert.InsertExpr (InsertExpr, insertExpr)
import Orville.PostgreSQL.Internal.Expr.Insert.InsertSource (InsertSource, insertSqlValues)
