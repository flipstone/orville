{- |
Module    : Orville.PostgreSQL.Internal.Expr.Update
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Update
  ( SetClauseList,
    setClauseList,
    SetClause,
    setColumn,
    UpdateExpr,
    updateExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.Update.SetClause (SetClause, setColumn)
import Orville.PostgreSQL.Internal.Expr.Update.SetClauseList (SetClauseList, setClauseList)
import Orville.PostgreSQL.Internal.Expr.Update.UpdateExpr (UpdateExpr, updateExpr)
